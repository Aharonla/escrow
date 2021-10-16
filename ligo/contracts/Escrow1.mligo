type rewardType = {
    name : string;
    slashing_rate : nat;
    commission_rate : nat;
    transfer_time_limit : int;
    confirm_time_limit : int;
}

type item = {
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
    index : nat;
    hash : string
}

type adminAction =
| AddType of rewardType
| ChangeType of rewardType
| RemoveType of rewardType
| Reward of nat

type action = 
| Admin of adminAction
| Offer of offerType
| Bid of nat
| Transfer of transferType
| Confirm of nat

type storage = {
    rewards : (string, rewardType) map;
    offered_items : (nat, item) map;
    sold_items : (nat, item) map;
    under_escrow : (nat, item) map;
    owner : address;
    last_id : nat;
}

type returnType = operation list * storage

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

let addType (t, store : rewardType * storage) : returnType =
    match Map.find_opt t.name store.rewards with 
    | Some tp -> (failwith("Reward type already exists") : returnType)
    | None -> ([] : operation list), { store with rewards = Map.add t.name t store.rewards }

let changeType (t, store : rewardType * storage) : returnType =
    match Map.find_opt t.name store.rewards with
    | None -> (failwith("Reward type doesn't exist") : returnType)
    | Some tp -> ([] : operation list), { store with rewards = Map.update t.name (Some t) store.rewards }
    
let removeType (t, store : rewardType * storage) : returnType =
    match Map.find_opt t.name store.rewards with
    | None -> (failwith("Reward type doesn't exist") : returnType)
    | Some tp -> ([] : operation list), { store with rewards = Map.remove t.name store.rewards }

let reward (i, store : nat * storage) : returnType =
    match Map.find_opt i store.under_escrow with
    | None -> (failwith("This item is not under escrow") : returnType)
    | Some item ->
    if Tezos.now <= item.end_time then
    (failwith("Wait until the exchange is over") : returnType)
    else
    let op_list : operation list = [] in 
    let buyer_contract = getContract item.buyer in
    let seller_contract = getContract item.seller in
    let updated_sold = Map.add i item store.sold_items in
    let updated_escrows = Map.remove i store.under_escrow in
    if item.state = "Confirmed" then
    let buyer_transfer = Tezos.transaction unit item.slashing buyer_contract :: op_list in
    let seller_transfer = Tezos.transaction unit (item.asked_price - item.commission) seller_contract :: op_list in
    op_list, store
    else 
    if item.state = "Waiting for validation" then
    let seller_transfer = Tezos.transaction unit (item.asked_price - item.commission) seller_contract :: op_list in
    op_list, store
    else  
    let buyer_transfer = Tezos.transaction unit (item.asked_price + item.slashing) buyer_contract :: op_list in
    op_list, store

    

// let terminateExchange (escrow, store : escrow * storage) : returnType =
//     ([] : operation list), store


//===========================================================================
// Entry Points
//===========================================================================

//===============================================
// Admin entry points
//===============================================

let admin (p, store : adminAction * storage) : returnType =
    if store.owner <> Tezos.source then
        (failwith("Only the owner has admin rights") : returnType)
    else 
        match p with
        | AddType(v) -> addType(v, store)
        | ChangeType(v) -> changeType(v, store)
        | RemoveType(v) -> removeType(v, store)
        | Reward(v) -> reward(v, store)

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

let offer (offer, store : offerType * storage) : returnType =
    match Map.find_opt offer.item_type store.rewards with 
    | None -> (failwith("This item's has type is not defined") : returnType)
    | Some r ->
    let new_item : item = {
        id = store.last_id + 1n;
        seller = Tezos.source;
        buyer = Tezos.source;
        item_type = offer.item_type;
        asked_price = offer.asked_price;
        commission = calculateCommissions(offer.asked_price, r.commission_rate);
        slashing = calculateCommissions(offer.asked_price, r.slashing_rate);
        state = "Inactive";
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
// -- index (nat)
// -- hash (string)
// conditions for execution:
// - the item is under escrow
// - only the seller can add item hash
// - the item was payed for
// - period time was not exceeded

let transfer (t, store : transferType * storage) : returnType =
    match Map.find_opt t.index store.under_escrow with
    | None -> (failwith("This item is not under escrow or doesn't exist") : returnType)
    | Some item -> 
    if Tezos.source <> item.seller then
    (failwith("Only the seller of the item can transfer the item hash") : returnType)
    else if item.state <> "Waiting for transfer" then
    (failwith("No tokens were sent for this item") : returnType)
    else if Tezos.now > item.end_time then
    (failwith("The transfer period is over. The transaction will be canceled shortly") : returnType)
    else
    let new_item = { item with 
    item_hash = t.hash; 
    state = "Waiting for validation"; 
    end_time = Tezos.now + item.confirm_period } in
    let new_store = { store with 
    under_escrow = Map.update t.index (Some new_item) store.under_escrow 
    } in
    ([] : operation list), new_store

//===============================================
// Buyer entry points
//===============================================

// the bid entry point is used by the buyer to pay for an offered item
// parameters:
// - i (nat)
// 

let bid (i, store : nat * storage) : returnType =
    let item = match Map.find_opt i store.offered_items with
    | None -> (failwith("No item matches the index you've chosen") : item)
    | Some item -> item in
    if Tezos.amount <= item.asked_price + item.slashing then
        (failwith("Not enough tokens were sent for this purchase") : returnType)
    else 
        let new_item  = { item with 
        state = "Waiting for transfer"; 
        buyer = Tezos.source; 
        end_time = Tezos.now + item.transfer_period 
        } in
        let updated_escrows = Map.add i new_item store.under_escrow in
        let updated_offers = Map.remove i store.offered_items in
        let new_store = { store with
        under_escrow = updated_escrows;
        offered_items = updated_offers;
        } in
    ([] : operation list), new_store

let confirm (i, store : nat * storage) : returnType =
    match Map.find_opt i store.under_escrow with
    | None -> (failwith("This item is not under escrow or doesnt exist") : returnType)
    | Some item -> 
    if Tezos.source <> item.buyer then
    (failwith("Only the item's buyer can confirm this transaction") : returnType)
    else if item.state <> "Waiting for validation" then
    (failwith("The state of the escrow doesn't require validation") : returnType)
    else if Tezos.now > item.end_time then
    (failwith("The confirmation period is over, slashing reward is lost") : returnType)
    else 
    let new_item = { item with 
    state = "Confirmed"; 
    end_time = Tezos.now 
    } in
    ([] : operation list), store

//===============================================
// Main
//===============================================


let main (a, store : action * storage) : returnType =
    match a with
    | Admin(v) -> admin(v, store)
    | Offer(v) -> offer(v, store)
    | Bid(v) -> bid(v, store)
    | Transfer(v) -> transfer(v, store)
    | Confirm(v) -> confirm(v, store)