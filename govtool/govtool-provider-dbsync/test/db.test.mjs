import { test } from 'node:test';
import assert from 'node:assert/strict';
import { EventEmitter } from 'node:events';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const { handleIdleErrors } = require('../dist/db.js');

test('an idle connection error is handled, not fatal', () => {
  const pool = new EventEmitter();
  const seen = [];
  handleIdleErrors(pool, (error) => seen.push(error));

  const cause = Object.assign(new Error('read ETIMEDOUT'), { code: 'ETIMEDOUT' });
  // Without a listener, EventEmitter throws on 'error' — which is what crashed the backend.
  assert.doesNotThrow(() => pool.emit('error', cause));
  assert.equal(seen.length, 1);
  assert.equal(seen[0].code, 'PROVIDER_UNAVAILABLE');
  assert.equal(seen[0].cause, cause);
});

test('the default handler warns without connection details', (t) => {
  const warn = t.mock.method(console, 'warn', () => {});
  const pool = new EventEmitter();
  handleIdleErrors(pool);
  pool.emit('error', Object.assign(new Error('connect to 10.0.0.1 failed'), { code: 'ETIMEDOUT' }));
  assert.equal(warn.mock.callCount(), 1);
  const message = warn.mock.calls[0].arguments[0];
  assert.match(message, /ETIMEDOUT/);
  assert.doesNotMatch(message, /10\.0\.0\.1/);
});

