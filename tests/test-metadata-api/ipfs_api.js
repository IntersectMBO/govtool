const crypto = require('crypto');
const express = require('express');
const fs = require('fs');
const path = require('path');

// A stand-in for an IPFS pinning service plus a path gateway, so an isolated
// test environment never reaches Pinata or a public gateway. Content is kept
// as one file per CID; the CID is what IPFS assigns content that fits in one
// block with raw leaves (CIDv1, raw codec, sha2-256, base32 `bafkrei...`),
// which is also what Pinata's v3 upload returns for GovTool metadata.

const BASE32 = 'abcdefghijklmnopqrstuvwxyz234567';

function base32(bytes) {
    let out = '';
    let bits = 0;
    let value = 0;
    for (const byte of bytes) {
        value = ((value << 8) | byte) & 0xffff;
        bits += 8;
        while (bits >= 5) {
            out += BASE32[(value >>> (bits - 5)) & 31];
            bits -= 5;
        }
    }
    if (bits > 0) out += BASE32[(value << (5 - bits)) & 31];
    return out;
}

function rawBlockCid(data) {
    const digest = crypto.createHash('sha256').update(data).digest();
    // CIDv1 (0x01), raw codec (0x55), sha2-256 (0x12), 32-byte digest (0x20).
    return 'b' + base32(Buffer.concat([Buffer.from([0x01, 0x55, 0x12, 0x20]), digest]));
}

// Only CIDs this service could have produced are looked up, so a request can
// never name a path outside the store.
const CID_PATTERN = /^bafkrei[a-z2-7]{52}$/;

const MAX_BYTES = 512 * 1024;

function contentTypeFor(data) {
    const text = data.toString('utf8');
    // A lossless round trip means the bytes are valid UTF-8.
    if (!Buffer.from(text, 'utf8').equals(data)) return 'application/octet-stream';
    try {
        JSON.parse(text);
        return 'application/json; charset=utf-8';
    } catch {
        return 'text/plain; charset=utf-8';
    }
}

function setup(app, options = {}) {
    const ipfsDir = options.ipfsDir;
    const maxBytes = options.maxBytes || MAX_BYTES;
    fs.mkdirSync(ipfsDir, { recursive: true });

    // Registered before the app-wide text parser so the bytes stay exact:
    // the CID and the on-chain blake2b hash are both over the raw body.
    app.use('/ipfs', express.raw({ type: () => true, limit: maxBytes }));

    const fileFor = (cid) => path.join(ipfsDir, cid);

    /**
     * @swagger
     * /ipfs:
     *   post:
     *     summary: Pin content and return its CID
     *     tags: [IPFS]
     *     requestBody:
     *       required: true
     *       content:
     *         application/octet-stream:
     *           schema:
     *             type: string
     *             format: binary
     *     responses:
     *       '201':
     *         description: Stored; the body is {"cid":"bafkrei..."}
     *       '400':
     *         description: Empty body
     *       '413':
     *         description: Over 512 KiB
     */
    const pin = (req, res) => {
        const body = Buffer.isBuffer(req.body) ? req.body : Buffer.alloc(0);
        if (body.length === 0) {
            return res.status(400).json({ message: 'Empty body' });
        }
        const cid = rawBlockCid(body);
        fs.writeFile(fileFor(cid), body, (err) => {
            if (err) {
                console.error(err);
                return res.status(500).json({ message: 'Failed to store content' });
            }
            res.status(201).json({ cid });
        });
    };
    app.post('/ipfs', pin);
    app.put('/ipfs', pin);

    /**
     * @swagger
     * /ipfs/{cid}:
     *   get:
     *     summary: Read pinned content, as a path gateway would
     *     tags: [IPFS]
     *     parameters:
     *       - in: path
     *         name: cid
     *         schema:
     *           type: string
     *         required: true
     *     responses:
     *       '200':
     *         description: The exact bytes that were pinned
     *       '404':
     *         description: Not pinned here
     */
    const read = (req, res) => {
        const cid = req.params.cid;
        if (!CID_PATTERN.test(cid)) {
            return res.status(404).json({ message: 'Not found' });
        }
        fs.readFile(fileFor(cid), (err, data) => {
            if (err) {
                return res.status(404).json({ message: 'Not found' });
            }
            res.set('Content-Type', contentTypeFor(data));
            res.set('Etag', `"${cid}"`);
            res.set('X-Ipfs-Path', `/ipfs/${cid}`);
            res.set('Cache-Control', 'public, max-age=29030400, immutable');
            res.status(200).send(data);
        });
    };
    app.get('/ipfs/:cid', read);
    // A trailing slash is how some clients join a gateway and a CID.
    app.get('/ipfs/:cid/', read);

    /**
     * @swagger
     * /ipfs/{cid}:
     *   delete:
     *     summary: Unpin content
     *     tags: [IPFS]
     *     parameters:
     *       - in: path
     *         name: cid
     *         schema:
     *           type: string
     *         required: true
     *     responses:
     *       '200':
     *         description: Unpinned, or was never pinned
     */
    app.delete('/ipfs/:cid', (req, res) => {
        const cid = req.params.cid;
        if (!CID_PATTERN.test(cid)) {
            return res.status(404).json({ message: 'Not found' });
        }
        fs.rm(fileFor(cid), { force: true }, (err) => {
            if (err) {
                console.error(err);
                return res.status(500).json({ message: 'Failed to unpin' });
            }
            res.status(200).json({ success: true });
        });
    });

    // Health for the pinning client; answers without touching the store.
    app.get('/ipfs', (req, res) => {
        res.status(200).json({ status: 'ok' });
    });

    // Body-parser failures (over the limit, aborted upload) as JSON, not a stack.
    // eslint-disable-next-line no-unused-vars
    app.use('/ipfs', (err, req, res, next) => {
        const status = err.status || err.statusCode || 500;
        res.status(status).json({
            message: status === 413 ? `Content is over ${maxBytes} bytes` : 'Upload failed',
        });
    });
}

module.exports = { setup, rawBlockCid, contentTypeFor };
