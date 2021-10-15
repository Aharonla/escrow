//  The Exchange structure represents any exchange initialized in the contract. It takes as parameters:
//  -  the seller of the goods
//  -  the buyer of the goods whose tokens are going to be held in the contract
//  -  the type of the exchange (by default in the demo it can only be a DOMAIN_NAME)
//  -  the timestamp of last update of the state of the exchange
//  -  a structure representing the total_escrow with the following fields: 
//   -  the amount sent to be stored in the contract
//   -  the calculated slashing amount: the incentive for the buyer to validate the exchange at the end of the escrow; If he validates the exchange he gets the slashing amount back, if not, he looses it.
//   -  the calculated commission amount: the commission on the escrow type that is sent to the contract owne at the end of the exchange 
//   -  the price of the goods set by the seller
// - the hash of the domain name

type exchange = {
    seller : address;
    buyer : address;
    state : string;
    exchange_type : string;
    last_update : timestamp;
    total_escrow : {
        escrow : tez;
        slashing : tez;
        commission : tez;
        asked_price : tez;
        shipping : tez;
    };
    domain_name : string;
}

type storage = {
    owner : address;
    slashing_rate : nat;
    exchange_types : (string, nat) map;
    exchange_states : string * string * string * string;
    exchanges : (string, exchange) map;
}

type returnType = operation list * storage

type updateTypeParams = string * nat

type changeOwnerParams = address

type addExchangeParams = address * string * tez * string

type validateTransmissionParams = address

type validateExchangeParams = address


type action = 
| UpdateExchangeType of updateTypeParams
| ChangeOwner of changeOwnerParams
| AddNewExchange of addExchangeParams
| ValidateSellerTransmission of validateTransmissionParams
| ValidateExchange of validateExchangeParams

// The updateExchangeType entry point allows the owner of the contract to 
// add a new exchange type or to update the existing ones. 
// It takes as parameters:
//     - The escrow type name
//     - The commission associated to the escrow type

let updateExchangeType (p, store : updateTypeParams * storage) : returnType =
    if store.owner <> Tezos.sender then 
        ([] : operation list), (failwith("only the owner can update the exchange type") : storage)
    else
        match Map.find_opt p.0 store.exchange_types with
        | Some t -> ([] : operation list), (failwith("type already exists") : storage)
        | None -> let new_store : storage = { store with exchange_types = Map.add p.0 p.1 store.exchange_types } in
        ([] : operation list), new_store


// The changeOwner entry point allows the owner of the contract defined in the storage to set a new owner
let changeOwner (p, store : changeOwnerParams * storage) : returnType =
    if store.owner <> Tezos.sender then
        ([] : operation list), (failwith("only the owner can set a new owner") : storage)
    else
        let new_store : storage = { store with owner = p } in
        ([] : operation list), new_store

// The addNewExhange entry point is used by a buyer to initialize a new exchange. It must fit the following statements:
//     - The exchange type must exist in the storage
//     - The amount sent by the buyer must be greater than the price set by the seller + the commission amount + the slashing amount
//     - The buyer should not have any ongoing exhanges
//     It takes the following paramets:
//     - The seller address
//     - The exchange type
//     - The price of the goods set by the buyer
//     - The hash of the domain name

let addNewExhange (p, store : addExchangeParams * storage) : returnType =
    ([] : operation list), store

// The validateSellerTransmission entry point is used by the owner of the contract to confirm that the seller transmitted the domain name code to the buyer. The owner can validate the seller transmission only of the exchanges that are awaiting for it. It takes the following parameters:
//     - The buyer address

let validateSellerTransmission (params, store : validateTransmissionParams * storage) : returnType =
    ([] : operation list), store

// The validateExchange entry point is used by the buyer or the owner of the contract to validate that the buyer received what he bought. The slashing amount held in the contract is collected by the caller of this function. The commission is sent to the owner of the contract, and the seller receives the price of the goods. Only the exchanges awaiting for validation can be validated. The function takes the following parameters:
//     - The buyer address

let validateExchange (params, store : validateExchangeParams * storage) : returnType =
    ([] : operation list), store


let main (action, store : action * storage) : operation list * storage =
    match action with 
    | UpdateExchangeType(v) -> updateExchangeType(v, store)
    | ChangeOwner(v) -> changeOwner(v, store)
    | AddNewExchange(v) -> ([] : operation list), store
    | ValidateSellerTransmission(v) -> ([] : operation list), store
    | ValidateExchange(v) -> ([] : operation list), store
    