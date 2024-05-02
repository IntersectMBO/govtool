const lock = {};

function acquireLock(key) {
    return new Promise((resolve, reject) => {
        if (!lock[key]) {
            lock[key] = true;
            resolve();
        } else {
            reject({ status: 423, message: 'Lock not available' });
        }
    });
}

function releaseLock(key) {
    lock[key] = false;
}

function setup(app) {
    /**
     * @swagger
     * tags:
     *   name: Locks
     *   description: API endpoints for managing locks
     */

    /**
     * @swagger
     * /lock/{key}:
     *   post:
     *     summary: Acquire lock
     *     tags: [Locks]
     *     parameters:
     *       - in: path
     *         name: key
     *         schema:
     *           type: string
     *         required: true
     *         description: The key of the lock to acquire
     *     responses:
     *       '200':
     *         description: Lock acquired successfully
     *       '423':
     *         description: Lock not available
     */
    app.post('/lock/:key', async (req, res) => {
        const key = req.params.key;
        try {
            await acquireLock(key);
            res.send('Lock acquired.');
        } catch (error) {
            res.status(error.status).send(error.message);
        }
    });

    /**
     * @swagger
     * /unlock/{key}:
     *   post:
     *     summary: Release lock
     *     tags: [Locks]
     *     parameters:
     *       - in: path
     *         name: key
     *         schema:
     *           type: string
     *         required: true
     *         description: The key of the lock to release
     *     responses:
     *       '200':
     *         description: Lock released successfully
     */
    app.post('/unlock/:key', (req, res) => {
        const key = req.params.key;
        releaseLock(key);
        res.send('Lock released.');
    });
}

module.exports.setup = setup;
