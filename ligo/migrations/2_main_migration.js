const { pkh } = require('../faucet.json');
const escrow = artifacts.require('Escrow1');
const { MichelsonMap } = require('@taquito/taquito');

module.exports = async (deployer, _network) => {
  const storage = {
    rewards: MichelsonMap.fromLiteral({}),
    offered_items: MichelsonMap.fromLiteral({}),
    sold_items: MichelsonMap.fromLiteral({}),
    under_escrow: MichelsonMap.fromLiteral({}),
    owner: pkh,
    last_id: 0,
  };
  deployer.deploy(escrow, storage);
};
