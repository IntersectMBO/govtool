const { test, before, after } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('fs');
const os = require('os');
const path = require('path');
const express = require('express');

const ipfs_api = require('../ipfs_api');

let server;
let base;
let ipfsDir;

before(async () => {
    ipfsDir = fs.mkdtempSync(path.join(os.tmpdir(), 'test-metadata-ipfs-'));
    const app = express();
    ipfs_api.setup(app, { ipfsDir, maxBytes: 1024 });
    // The real app parses text bodies after the IPFS routes; keep that order.
    app.use(express.text());
    await new Promise((resolve) => {
        server = app.listen(0, '127.0.0.1', resolve);
    });
    base = `http://127.0.0.1:${server.address().port}`;
});

after(() => {
    server.close();
    fs.rmSync(ipfsDir, { recursive: true, force: true });
});

test('rawBlockCid matches the CIDs IPFS assigns raw single-block content', () => {
    assert.equal(
        ipfs_api.rawBlockCid(Buffer.alloc(0)),
        'bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku',
    );
    assert.equal(
        ipfs_api.rawBlockCid(Buffer.from('hello world')),
        'bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e',
    );
});

test('POST /ipfs pins the exact bytes and GET /ipfs/:cid serves them', async () => {
    const body = '{"hashAlgorithm":"blake2b-256","body":{"comment":"ok"}}';
    const pinned = await fetch(`${base}/ipfs`, {
        method: 'POST',
        headers: { 'Content-Type': 'text/plain' },
        body,
    });
    assert.equal(pinned.status, 201);
    const { cid } = await pinned.json();
    assert.equal(cid, ipfs_api.rawBlockCid(Buffer.from(body)));

    const read = await fetch(`${base}/ipfs/${cid}`);
    assert.equal(read.status, 200);
    assert.match(read.headers.get('content-type'), /^application\/json/);
    assert.equal(await read.text(), body);

    const slash = await fetch(`${base}/ipfs/${cid}/`);
    assert.equal(slash.status, 200);
});

test('PUT /ipfs pins too, and non-UTF-8 bytes come back unchanged', async () => {
    const bytes = Buffer.from([0xff, 0x00, 0xfe, 0x01]);
    const pinned = await fetch(`${base}/ipfs`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/octet-stream' },
        body: bytes,
    });
    assert.equal(pinned.status, 201);
    const { cid } = await pinned.json();
    const read = await fetch(`${base}/ipfs/${cid}`);
    assert.equal(read.headers.get('content-type'), 'application/octet-stream');
    assert.deepEqual(Buffer.from(await read.arrayBuffer()), bytes);
});

test('plain text is served as text/plain', async () => {
    const pinned = await fetch(`${base}/ipfs`, { method: 'POST', body: 'not json' });
    const { cid } = await pinned.json();
    const read = await fetch(`${base}/ipfs/${cid}`);
    assert.match(read.headers.get('content-type'), /^text\/plain/);
});

test('an empty body is refused', async () => {
    const res = await fetch(`${base}/ipfs`, { method: 'POST' });
    assert.equal(res.status, 400);
});

test('a body over the limit is refused', async () => {
    const res = await fetch(`${base}/ipfs`, { method: 'POST', body: 'x'.repeat(2048) });
    assert.equal(res.status, 413);
});

test('unknown and malformed CIDs are 404 and never touch the filesystem', async () => {
    const unknown = ipfs_api.rawBlockCid(Buffer.from('never pinned'));
    assert.equal((await fetch(`${base}/ipfs/${unknown}`)).status, 404);
    for (const bad of ['..', '%2e%2e%2fpackage.json', 'QmNotOurs', 'BAFKREI']) {
        assert.equal((await fetch(`${base}/ipfs/${bad}`)).status, 404, bad);
    }
});

test('DELETE /ipfs/:cid unpins', async () => {
    const pinned = await fetch(`${base}/ipfs`, { method: 'POST', body: 'to delete' });
    const { cid } = await pinned.json();
    assert.equal((await fetch(`${base}/ipfs/${cid}`, { method: 'DELETE' })).status, 200);
    assert.equal((await fetch(`${base}/ipfs/${cid}`)).status, 404);
});

test('GET /ipfs answers health', async () => {
    const res = await fetch(`${base}/ipfs`);
    assert.equal(res.status, 200);
    assert.deepEqual(await res.json(), { status: 'ok' });
});
