type rewardType = {
    [@layout:comb]
    name : string;
    slashing_rate : nat;
    commission_rate : nat;
    transfer_time_limit : int;
    confirm_time_limit : int;
}

type item = {
    [@layout:comb]
    id : nat;
    seller : address;
    buyer : address;
    item_type : string;
    asked_price : tez;
    commission : tez;
    slashing : tez;
    state : string;
    start_time : timestamp;
    transfer_period : int;
    confirm_period : int;
    end_time : timestamp;
    item_hash : string;
}

type offerType = {
    item_type : string;
    asked_price : tez;
}

type transferType = {
    id : nat;
    hash : string
}


type action = 
| AddType of rewardType
| ChangeType of rewardType
| RemoveType of rewardType
| Reward of nat
| Offer of offerType
| Bid of nat
| Transfer of transferType
| Confirm of nat
| Deny of nat

type storage = {
    [@layout:comb]
    rewards : (string, rewardType) map;
    offered_items : (nat, item) map;
    sold_items : (nat, item) map;
    under_escrow : (nat, item) map;
    owner : address;
    last_id : nat;
}

type returnType = operation list * storage

let states = ("Inactive", "Waiting for transfer", "Waiting for validation", "Confirmed", "Denied")

//===========================================================================
// Helper functions
//===========================================================================

let calculateCommissions (price, rate : tez * nat) : tez =
    price * rate / 100n

let getContract (ad : address) : unit contract =
    let contr : unit contract = match ((Tezos.get_contract_opt ad) : (unit contract) option) with
    | None -> (failwith("No contract to matches transaction") : unit contract)
    | Some c -> c in
    contr

//==========================================================
// Admin functions
//==========================================================




//===========================================================================
// Entry Points
//===========================================================================

//===============================================
// Admin entry point
//===============================================

// the admin's 'type' functions are used to manage the rewarding system

let addType (t, store : rewardType * storage) : returnType =
    // !!!-REMOVE COMMENTED LINES (3 FOLLOWING LINES) WHEN NOT TESTING-!!!
    // if Tezos.source <> store.owner then
    //     (failwith("Only the contract's owner has admin rights"))
    // else
    match Map.find_opt t.name store.rewards with 
    | Some tp -> (failwith("Reward type already exists") : returnType)
    | None -> ([] : operation list), { store with rewards = Map.add t.name t store.rewards }

let changeType (t, store : rewardType * storage) : returnType =
    // !!!-REMOVE COMMENTED LINES (3 FOLLOWING LINES) WHEN NOT TESTING-!!!
    // if Tezos.source <> store.owner then
    //     (failwith("Only the contract's owner has admin rights"))
    // else
    match Map.find_opt t.name store.rewards with
    | None -> (failwith("Reward type doesn't exist") : returnType)
    | Some tp -> ([] : operation list), { store with rewards = Map.update t.name (Some t) store.rewards }
    
let removeType (t, store : rewardType * storage) : returnType =
    // !!!-REMOVE COMMENTED LINES (3 FOLLOWING LINES) WHEN NOT TESTING-!!!
    // if Tezos.source <> store.owner then
    //     (failwith("Only the contract's owner has admin rights"))
    // else
    match Map.find_opt t.name store.rewards with
    | None -> (failwith("Reward type doesn't exist") : returnType)
    | Some tp -> ([] : operation list), { store with rewards = Map.remove t.name store.rewards }

// the reward entry point hands out rewards as follows:
// [1] If all stages of the transaction were followed through by both the buyer and the seller:
// -- The seller gets his asked price minus the commission collected by the contract.
// -- The buyer is rewarded for confirming the transmition by getting his added 'slashing' fee.
// [2] If the buyer didn't confirm the transmition on time:
// -- The seller gets his asked price minus the commission collected by the contract.
// -- The slashing fee is collected by the contract as a penalty.
// [3] If either the seller didn't transmit the item, or the buyer denied the transmition:
// -- The buyer gets all money back. 

let reward (i, store : nat * storage) : returnType =
    // !!!-REMOVE COMMENTED LINES (3 FOLLOWING LINES) WHEN NOT TESTING-!!! 
    // if Tezos.source <> store.owner then
    //     (failwith("Only the contract's owner can hand out rewards"))
    // else
    match Map.find_opt i store.under_escrow with
    | None -> (failwith("This item is not under escrow") : returnType)
    | Some item ->
    if Tezos.now <= item.end_time then
        (failwith("Wait until the exchange is over") : returnType)
    else
        let buyer_contract = getContract item.buyer in
        let seller_contract = getContract item.seller in
        let new_store = { store with
        sold_items = Map.add i item store.sold_items;
        under_escrow = Map.remove i store.under_escrow;
        } in
    if item.state = states.3 then
        let buyer_transfer = Tezos.transaction unit item.slashing buyer_contract in
        let seller_transfer = Tezos.transaction unit (item.asked_price - item.commission) seller_contract in
        let op_list : operation list = [buyer_transfer; seller_transfer] in
        op_list, new_store
    else if item.state = states.2 then
        let seller_transfer = Tezos.transaction unit (item.asked_price - item.commission) seller_contract in
        let op_list : operation list = [seller_transfer] in
        op_list, new_store
    else  
    let buyer_transfer = Tezos.transaction unit (item.asked_price + item.slashing) buyer_contract in
    let op_list : operation list = [buyer_transfer] in
    op_list, new_store


//===============================================
// Seller entry points
//===============================================

// the offer entry point is used by a seller to add an item to the market place.
// parameters:
// - offerType (record) of:
// -- item_type (string)  
// -- asked_price (tez). 
// conditions for execution:
// - item_type is a type defined in store.rewards
// - asked_price needs to be positive

let offer (offer, store : offerType * storage) : returnType =
    match Map.find_opt offer.item_type store.rewards with 
    | None -> (failwith("This item's type is not defined") : returnType)
    | Some r ->
    if offer.asked_price = 0tez then
        (failwith("Item price is 0tez") : returnType)
    else
    let new_item : item = {
        id = store.last_id + 1n;
        seller = Tezos.source;
        buyer = Tezos.source;
        item_type = offer.item_type;
        asked_price = offer.asked_price;
        commission = calculateCommissions(offer.asked_price, r.commission_rate);
        slashing = calculateCommissions(offer.asked_price, r.slashing_rate);
        state = states.0;
        item_hash = "";
        start_time = Tezos.now;
        transfer_period = r.transfer_time_limit;
        confirm_period = r.confirm_time_limit;
        end_time = (0 : timestamp);
    } in
    let new_store = { store with 
    offered_items = Map.add new_item.id new_item store.offered_items; 
    last_id = store.last_id + 1n 
    } in
    ([] : operation list), new_store

// the transfer entry point is used by the seller to add the bought item hash to the contract.
// parameters:
// - transferType (record) of:
// -- id (nat)
// -- hash (string)
// conditions for execution:
// - the item is under escrow
// - only the seller can add item hash
// - the item was payed for
// - period time was not exceeded

let transfer (t, store : transferType * storage) : returnType =
    match Map.find_opt t.id store.under_escrow with
    | None -> (failwith("This item is not under escrow or doesn't exist") : returnType)
    | Some item -> 
    if Tezos.source <> item.seller then
        (failwith("Only the seller of the item can transfer the item hash") : returnType)
    else if item.state <> states.1 then
        (failwith("No tokens were sent for this item") : returnType)
    else if Tezos.now > item.end_time then
        (failwith("The transfer period is over. The transaction will be canceled shortly") : returnType)
    else
    let new_item = { item with 
    item_hash = t.hash; 
    state = states.2; 
    end_time = Tezos.now + item.confirm_period } in
    let new_store = { store with 
    under_escrow = Map.update t.id (Some new_item) store.under_escrow 
    } in
    ([] : operation list), new_store

//===============================================
// Buyer entry points
//===============================================

// the bid entry point is used by the buyer to pay for an offered item
// parameters:
// - id (nat)
// conditions:
// - the id has to match with an offered item's id
// - the amount sent has to match the price of the bought item

let bid (id, store : nat * storage) : returnType =
    let item = match Map.find_opt id store.offered_items with
    | None -> (failwith("No item matches the id you've chosen") : item)
    | Some item -> item in
    if Tezos.amount < item.asked_price + item.slashing then
        (failwith("Not enough tokens were sent for this purchase") : returnType)
    else 
    let new_item  = { item with 
    state = states.1; 
    buyer = Tezos.source; 
    end_time = Tezos.now + item.transfer_period 
    } in
    let new_store = { store with
    under_escrow = Map.add id new_item store.under_escrow;
    offered_items = Map.remove id store.offered_items;
    } in
    ([] : operation list), new_store

// the confirm entry point is used to confirm that the item was received and satisfied the buyer's requirements
// this entry point terminates the escrow
// parameters:
// - id (nat)
// conditions:
// - the id points to an item that is currently under escrow
// - the confirming address is this item's buyer
// - all preceeding periods were completed
// - confirmation period is ongoing

let confirm (id, store : nat * storage) : returnType =
    match Map.find_opt id store.under_escrow with
    | None -> (failwith("This item is not under escrow or doesn't exist") : returnType)
    | Some item -> 
    if Tezos.source <> item.buyer then
        (failwith("Only the item's buyer can confirm this transaction") : returnType)
    else if item.state <> states.2 then
        (failwith("The state of the escrow doesn't require validation") : returnType)
    else if Tezos.now > item.end_time then
        (failwith("The confirmation period is over") : returnType)
    else 
    let new_item = { item with 
    state = states.3; 
    end_time = Tezos.now 
    } in
    let new_store = { store with
    under_escrow = Map.update id (Some new_item) store.under_escrow;
    } in
    ([] : operation list), new_store

// the deny entry point is used to state that the item was received but didn't satisfy the buyer's requirements
// in which case the transaction terminates and the buyer gets their money back
// parameters:
// - id (nat)
// conditions:
// - the id points to an item that is currently under escrow
// - the confirming address is this item's buyer
// - all preceeding periods were completed
// - confirmation period is ongoing

let deny (id, store : nat * storage) : returnType =
    match Map.find_opt id store.under_escrow with
    | None -> (failwith("This item is not under escrow or doesn't exist") : returnType)
    | Some item -> 
    if Tezos.source <> item.buyer then
        (failwith("Only the item's buyer can deny this transaction") : returnType)
    else if item.state <> states.2 then
        (failwith("The state of the escrow doesn't require validation") : returnType)
    else if Tezos.now > item.end_time then
        (failwith("The confirmation period is over") : returnType)
    else
    let new_item = { item with
    state = states.4;
    end_time = Tezos.now
    } in
    let new_store = {store with 
    under_escrow = Map.update id (Some new_item) store.under_escrow;
    } in
    ([] : operation list), new_store

//===========================================================================
// Main
//===========================================================================


let main (a, store : action * storage) : returnType =
    match a with
    | Offer(v) -> offer(v, store)
    | Bid(v) -> bid(v, store)
    | Transfer(v) -> transfer(v, store)
    | Confirm(v) -> confirm(v, store)
    | Deny(v) -> deny(v, store)
    | Reward(v) -> reward(v, store)
    | AddType(v) -> addType(v, store)
    | ChangeType(v) -> changeType(v, store)
    | RemoveType(v) -> removeType(v, store)