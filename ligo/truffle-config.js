const { mnemonic, secret, password, email } = require('./faucet.json');
const { alice } = require('./scripts/sandbox/accounts');

module.exports = {
  networks: {
    development: {
      host: 'http://localhost',
      port: 20000,
      network_id: '*',
      secretKey: alice.sk,
      type: 'tezos',
    },
    testnet: {
      host: 'https://testnet-tezos.giganode.io',
      port: 8732, //172.15.67.251.55983',
      network_id: '*',
      secret,
      mnemonic,
      password,
      email,
      type: 'tezos',
      networkCheckTimeout: 20000,
    },
  },
};
