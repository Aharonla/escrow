#include "Escrow2.mligo"

let initial_storage = {
        rewards = (Map.empty : (string, rewardType) map);
        offered_items = (Map.empty : (nat, item) map);
        sold_items = (Map.empty : (nat, item) map);
        under_escrow = (Map.empty : (nat, item) map);
        owner = ("tz1azRiBTeZQMq41ye6dJTKNsfdqXQKSKEho" : address);
        last_id = 0n;
    }

let rwd = {
        name = "name";
        slashing_rate = 1n;
        commission_rate = 1n;
        transfer_time_limit = 10;
        confirm_time_limit = 10;
    }

let (taddr,_,_) = Test.originate main initial_storage 0tez

let test_storage =
    assert (Test.get_storage taddr = initial_storage)

// admin type 

let test_addType =
    let store_add = { initial_storage with rewards = Map.add rwd.name rwd initial_storage.rewards} in
    let contr = Test.to_contract taddr in
    let () = Test.transfer_to_contract_exn contr (AddType (rwd)) 0tez in
    assert (Test.get_storage taddr = store_add)

let test_changeType =
    let rwd_change = { rwd with transfer_time_limit = 300; confirm_time_limit = 300} in
    let store_change = {initial_storage with rewards = Map.update rwd_change.name (Some rwd_change) initial_storage.rewards} in
    let contr = Test.to_contract taddr in
    let () = Test.transfer_to_contract_exn contr (ChangeType (rwd_change)) 0tez in
    assert (Test.get_storage taddr = store_change)

let test_removeType = 
    let contr = Test.to_contract taddr in
    let () = Test.transfer_to_contract_exn contr (RemoveType (rwd)) 0tez in
    log Tezos.source
    assert (Test.get_storage taddr = initial_storage)

let trans_rwd = {
        name = "name";
        slashing_rate = 1n;
        commission_rate = 1n;
        transfer_time_limit = 300;
        confirm_time_limit = 300;
    }

let trans_storage = { initial_storage with 
    rewards = Map.add trans_rwd.name trans_rwd initial_storage.rewards
    }

// let trans_item = { 
//     id = 1n;
//     seller : address;
//     buyer : address;
//     item_type : string;
//     asked_price : tez;
//     commission : tez;
//     slashing : tez;
//     state : string;
//     start_time : timestamp;
//     transfer_period : int;
//     confirm_period : int;
//     end_time : timestamp;
//     item_hash : string; 
//     }

// let (taddr,_,_) = Test.originate main trans_storage 0tez

// let test_trans_storage =
//     assert (Test.get_storage taddr = trans_storage)

// let test_offer =

