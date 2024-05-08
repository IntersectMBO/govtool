const { v4: uuidv4 } = require('uuid');
const lock = {};

function acquireLock(key, expiry_secs = 180) {
    const now = Date.now();
    if (!lock[key] || lock[key].expiry < now) {
        const uuid = uuidv4();
        lock[key] = {
            locked: true,
            expiry: now + expiry_secs * 1000,
            uuid: uuid,
        };
        return uuid
    }
}
function releaseLock(key,uuid) {
    if(uuid){
        _lock=lock[key]
        if(_lock && (_lock.uuid != uuid)){
            // if the uuid doesn't match, the lock should
           // have expired and obtained by process.
            return;
        }
    }
    delete lock[key];
}


function setup(app) {
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
     *       - in: query
     *         name: expiry_secs
     *         schema:
     *           type: integer
     *           minimum: 1
     *           default: 180
     *         description: The expiration time of the lock in seconds (default is 180s)
     *     responses:
     *       '200':
     *         description: Lock acquired successfully
     *         content:
     *           application/json:
     *             schema:
     *               type: object
     *               properties:
     *                 uuid:
     *                   type: string
     *                   description: The UUID of the acquired lock
     *       '423':
     *         description: Lock not available
     */
    app.post('/lock/:key', (req, res) => {
        const key = req.params.key;
        const expiry_secs = req.query.expiry_secs ? parseInt(req.query.expiry_secs) : 180;
        const lock_uuid=acquireLock(key, expiry_secs)
        if(lock_uuid){
            res.json({ uuid: lock_uuid })
        }else{
            res.status(423).json({ status: 423, message: 'Lock not available' });

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
     *       - in: query
     *         name: uuid
     *         schema:
     *           type: string
     *         required: false
     *         description: The UUID of the lock to release
     *     responses:
     *       '200':
     *         description: Lock released successfully
     */
    app.post('/unlock/:key', (req, res) => {
        const key = req.params.key;
        const uuid = req.query.uuid;

        releaseLock(key, uuid);
        res.send('Lock released.');

    });
}

module.exports.setup = setup;
