const { strictEqual } = require('assert');
const assert = require('assert');
const { pkh } = require('../faucet.json');

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
      assert.strictEqual(storage.offered_items.size, 0);
      assert.strictEqual(storage.sold_items.size, 0);
      assert.strictEqual(storage.under_escrow.size, 0);
      assert.strictEqual(storage.owner, pkh);
      assert.strictEqual(storage.last_id, 0);
    });

    it('Should add a reward type by an account different from the owner account', async function () {
      try {
        await instance.admin(
          addType({
            name: 'name',
            slashing_rate: 1,
            commission_rate: 1,
            transfer_time_limit: 1,
            confirm_time_limit: 1,
          })
        );
      } catch (error) {
        assert.strictEqual(error, 'Only the owner has admin rights');
      }
    });

    it('Should add a reward type successfully', async function () {
      await instance.admin(
        addType({
          name: 'name',
          slashing_rate: 1,
          commission_rate: 1,
          transfer_time_limit: 1,
          confirm_time_limit: 1,
        })
      );
      const storage = await instance.storage();
      assert.strictEqual(storage.rewards, {
        name: 'name',
        slashing_rate: 1,
        commission_rate: 1,
        transfer_time_limit: 1,
        confirm_time_limit: 1,
      });
    });

    it('Should add an already existing reward type', async function () {
      try {
        await instance.admin(
          addType({
            name: 'name',
            slashing_rate: 1,
            commission_rate: 1,
            transfer_time_limit: 1,
            confirm_time_limit: 1,
          })
        );
      } catch (error) {
        assert.strictEqual(error, 'Reward type already exists');
      }
    });

    it('Should change a non existing reward type', async function () {
      try {
        await instance.admin(
          changeType({
            name: 'differentName',
            slashing_rate: 1,
            commission_rate: 1,
            transfer_time_limit: 30,
            confirm_time_limit: 30,
          })
        );
      } catch (error) {
        assert.strictEqual(error, "Reward type doesn't exist");
      }
    });

    it('Should change a reward type successfully', async function () {
      await instance.admin(
        changeType({
          name: 'name',
          slashing_rate: 1,
          commission_rate: 1,
          transfer_time_limit: 30,
          confirm_time_limit: 30,
        })
      );
      let storage = await instance.storage();
      assert.strictEqual(storage.transfer_time_limit, 30);
      assert.strictEqual(storage.confirm_time_limit, 30);
    });

    it('Should try to remove a non existing type', async function () {
      try {
        await instance.admin(removeType('differentName'));
      } catch (error) {
        assert.strictEqual(error, "Reward type doesn't exist");
      }
    });

    it('Should try to remove an existing type', async function () {
      await instance.admin(
        addType({
          name: 'differentName',
          slashing_rate: 1,
          commission_rate: 1,
          transfer_time_limit: 1,
          confirm_time_limit: 1,
        })
      );
      await instance.admin(removeType('differentName'));
      await instance.storage();
      assert.strictEqual(storage.rewards.size, 1);
    });
  });
};
