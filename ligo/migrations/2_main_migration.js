const { alice, bob } = require('../scripts/sandbox/accounts');
const escrow = artifacts.require('Escrow1');
const { MichelsonMap } = require('@taquito/taquito');

module.exports = async (deployer, _network) => {
  const storage = {
    rewards: MichelsonMap.fromLiteral({}), //(string, rewardType) map,
    offered_items: MichelsonMap.fromLiteral({}), //(nat, item) map,
    sold_items: MichelsonMap.fromLiteral({}), //(nat, item) map,
    under_escrow: MichelsonMap.fromLiteral({}), //(nat, item) map,
    count: 0, //nat,
    owner: alice.pkh, //address,
    last_id: 0, //: nat,
  };
  deployer.deploy(escrow, storage);
};
