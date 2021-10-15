const assert = require('assert');

() => {
  const contractName = 'Escrow1';
  const Escrow1 = artifacts.require(contractName);

  contract(contractName, async function () {
    let instance = null;

    async () => {
      instance = await Escrow1.deployed();
    };
    // First test - check if the storage was deployed correctly
    it('Should check the initial storage', async function () {
      const storage = await instance.storage();
      console.log(storage);
      assert.strictEqual(storage.rewards.size, 0);
    });
  });
};
