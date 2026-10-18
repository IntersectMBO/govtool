'use strict';

// The three Blockfrost endpoints the GovTool test suites call, answered
// from a local devnet:
//
//   POST /v0/tx/submit                  forwarded to Kuber's cardano-submit-api
//                                       compatible endpoint (POST /api/submit/tx)
//   GET  /v0/accounts/{stake_address}   from db-sync
//   GET  /v0/governance/dreps/{drep_id} from db-sync
//
// Paths are accepted with or without a leading /api, so both
// BLOCKFROST_URL=http://host:port and http://host:port/api work. The
// project_id header is ignored. Anything else answers 404.

const { parseStakeAddress, parseDRepId, cip129DRepId } = require('./ids');
const queries = require('./queries');

const MAX_TX_BYTES = 64 * 1024;

const STATUS_TEXT = {
  400: 'Bad Request',
  404: 'Not Found',
  405: 'Method Not Allowed',
  413: 'Payload Too Large',
  415: 'Unsupported Media Type',
  500: 'Internal Server Error',
  502: 'Bad Gateway',
};

class HttpError extends Error {
  constructor(status, message) {
    super(message);
    this.status = status;
  }
}

function sendJson(res, status, body) {
  const payload = JSON.stringify(body);
  res.writeHead(status, {
    'Content-Type': 'application/json',
    'Content-Length': Buffer.byteLength(payload),
  });
  res.end(payload);
}

function sendError(res, status, message) {
  sendJson(res, status, {
    status_code: status,
    error: STATUS_TEXT[status] || 'Error',
    message,
  });
}

function readBody(req, limit) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on('data', (chunk) => {
      size += chunk.length;
      if (size > limit) {
        reject(new HttpError(413, `Transaction exceeds ${limit} bytes.`));
        req.destroy();
        return;
      }
      chunks.push(chunk);
    });
    req.on('end', () => resolve(Buffer.concat(chunks)));
    req.on('error', reject);
  });
}

function numberOrNull(value) {
  return value === null || value === undefined ? null : Number(value);
}

async function submitTx(req, res, deps) {
  const type = (req.headers['content-type'] || '').split(';')[0].trim().toLowerCase();
  if (type !== 'application/cbor') {
    throw new HttpError(415, 'Content-Type must be application/cbor.');
  }
  const body = await readBody(req, MAX_TX_BYTES);
  if (body.length === 0) throw new HttpError(400, 'Empty transaction body.');

  let upstream;
  try {
    upstream = await deps.fetch(`${deps.kuberUrl}/api/submit/tx`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/cbor', Accept: 'application/json' },
      body,
      signal: AbortSignal.timeout(deps.submitTimeoutMs),
    });
  } catch (err) {
    throw new HttpError(502, `Kuber is unreachable: ${err.message}`);
  }
  const text = await upstream.text();
  if (upstream.status >= 200 && upstream.status < 300) {
    // Kuber answers 202 with the tx id as a JSON string; Blockfrost answers
    // 200 with the same body.
    let txId;
    try {
      txId = JSON.parse(text);
    } catch {
      txId = text.trim();
    }
    if (typeof txId !== 'string' || !/^[0-9a-f]{64}$/i.test(txId)) {
      throw new HttpError(502, `Unexpected Kuber submit response: ${text.slice(0, 200)}`);
    }
    sendJson(res, 200, txId.toLowerCase());
    return;
  }
  // A node rejection: Blockfrost reports it as 400 with the reason.
  let message = text;
  try {
    const parsed = JSON.parse(text);
    message = parsed.message || parsed.error || text;
    if (typeof message !== 'string') message = JSON.stringify(message);
  } catch {
    // Plain-text error from Kuber.
  }
  sendError(res, upstream.status >= 500 ? 400 : upstream.status, message);
}

async function getAccount(res, stakeAddress, deps) {
  const parsed = parseStakeAddress(stakeAddress);
  if (!parsed) throw new HttpError(400, 'Invalid or malformed stake address format.');
  const row = await queries.account(deps.db, parsed.hashRaw);
  if (!row) throw new HttpError(404, 'The requested component has not been found.');

  let dRepId = null;
  if (row.drep_raw) {
    dRepId = cip129DRepId(row.drep_raw, row.drep_has_script);
  } else if (row.drep_view) {
    // drep_always_abstain / drep_always_no_confidence
    dRepId = row.drep_view;
  }
  // Only the fields the shim can answer exactly; amounts are not served.
  sendJson(res, 200, {
    stake_address: row.stake_address,
    active: Boolean(row.registered && row.pool_id),
    active_epoch: row.registered ? numberOrNull(row.active_epoch) : null,
    registered: Boolean(row.registered),
    pool_id: row.registered ? row.pool_id : null,
    drep_id: row.registered ? dRepId : null,
  });
}

async function getDRep(res, id, deps) {
  const parsed = parseDRepId(id);
  if (!parsed) throw new HttpError(400, 'Invalid or malformed DRep id format.');
  const row = await queries.dRep(deps.db, parsed.raw, parsed.hasScript);
  if (!row) throw new HttpError(404, 'The requested component has not been found.');
  const expired =
    row.active_until !== null &&
    row.current_epoch !== null &&
    Number(row.active_until) < Number(row.current_epoch);
  sendJson(res, 200, {
    drep_id: cip129DRepId(row.raw, row.has_script),
    hex: Buffer.from(row.raw).toString('hex'),
    amount: row.amount || '0',
    active: !row.retired,
    active_epoch: numberOrNull(row.active_epoch),
    has_script: Boolean(row.has_script),
    retired: Boolean(row.retired),
    expired,
    last_active_epoch: null,
  });
}

function createHandler(deps) {
  const options = { submitTimeoutMs: 30000, fetch: globalThis.fetch, ...deps };

  return async function handle(req, res) {
    try {
      const url = new URL(req.url, 'http://shim.invalid');
      const path = url.pathname.replace(/^\/api(?=\/)/, '').replace(/\/+$/, '');

      if (path === '/health' && req.method === 'GET') {
        sendJson(res, 200, { is_healthy: true });
        return;
      }
      if (path === '/v0/tx/submit') {
        if (req.method !== 'POST') throw new HttpError(405, 'Use POST.');
        await submitTx(req, res, options);
        return;
      }
      let m = /^\/v0\/accounts\/([^/]+)$/.exec(path);
      if (m) {
        if (req.method !== 'GET') throw new HttpError(405, 'Use GET.');
        await getAccount(res, decodeURIComponent(m[1]), options);
        return;
      }
      m = /^\/v0\/governance\/dreps\/([^/]+)$/.exec(path);
      if (m) {
        if (req.method !== 'GET') throw new HttpError(405, 'Use GET.');
        await getDRep(res, decodeURIComponent(m[1]), options);
        return;
      }
      throw new HttpError(404, 'The requested component has not been found.');
    } catch (err) {
      if (err instanceof HttpError) {
        sendError(res, err.status, err.message);
      } else {
        // Details go to the log, not to the client.
        console.error(err);
        sendError(res, 500, 'An unexpected response was received from the backend.');
      }
    }
  };
}

module.exports = { createHandler };
