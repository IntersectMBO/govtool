#!/usr/bin/env node
'use strict';

// Derives the Playwright faucet settings (FAUCET_ADDRESS,
// FAUCET_PAYMENT_PRIVATE, FAUCET_STAKE_PRIVATE) from the devnet faucet's
// cardano-cli signing key, and writes them to <state dir>/faucet.env.
//
// The suite's faucet is a payment + stake key pair (lib/wallet/testWallets.ts
// faucetWallet) whose base address holds the funds. A devnet faucet is
// usually a payment key alone, funded at its enterprise address. Without a
// stake key file this script generates one (kept in the state dir, so later
// runs reuse it), and --fund moves FAUCET_FUND_ADA from the enterprise
// address to the base address (built by Kuber, signed here, submitted to
// Kuber); the rest stays for adaup's chain seed, which spends from the
// enterprise address.
//
// Key formats accepted (the JSON cardano-cli writes):
//   cborHex 5820 + 32-byte key     (PaymentSigningKeyShelley_ed25519)
//   cborHex 5880 + 128-byte key    (PaymentExtendedSigningKeyShelley_ed25519_bip32);
//                                  its first 64 bytes are the extended private key
// FAUCET_*_PRIVATE is the hex of those 32 or 64 bytes, which is what
// Ed25519KeyAsync.fromPrivateKeyHex reads.
//
// Usage (needs tests/govtool-frontend/playwright/node_modules):
//   node tests/devnet/scripts/faucet-env.js [--fund]
// Settings from the environment (up.sh exports .env.devnet):
//   FAUCET_SKEY_FILE, FAUCET_STAKE_SKEY_FILE (optional), DEVNET_OUTPUT_DIR,
//   KUBER_URL, DEVNET_STATE_DIR (default tests/devnet/.state)
//
// Private keys go to the state file only (mode 0600), never to stdout.

const crypto = require('node:crypto');
const fs = require('node:fs');
const path = require('node:path');
const { createRequire } = require('node:module');

const repoRoot = path.resolve(__dirname, '../../..');
const playwrightDir = path.join(repoRoot, 'tests/govtool-frontend/playwright');
const requireFromPlaywright = createRequire(path.join(playwrightDir, 'package.json'));

let libcardano;
let libcardanoWallet;
let kuberClient;
try {
  libcardano = requireFromPlaywright('libcardano');
  libcardanoWallet = requireFromPlaywright('libcardano-wallet');
  kuberClient = requireFromPlaywright('kuber-client');
} catch (err) {
  console.error(`Run npm ci in ${playwrightDir} first (${err.message}).`);
  process.exit(1);
}
const { Ed25519KeyAsync } = libcardano;
const { ShelleyWallet, SimpleCip30Wallet } = libcardanoWallet;
const { KuberApiProvider } = kuberClient;

const NETWORK_ID = 0;

function resolveKeyPath(file) {
  if (!file) return null;
  if (path.isAbsolute(file)) return file;
  return path.join(process.env.DEVNET_OUTPUT_DIR || '.', file);
}

/** The hex FAUCET_*_PRIVATE expects, from a cardano-cli .skey file. */
function privateKeyHex(file) {
  const json = JSON.parse(fs.readFileSync(file, 'utf8'));
  const cbor = String(json.cborHex || '').toLowerCase();
  if (/^5820[0-9a-f]{64}$/.test(cbor)) return cbor.slice(4);
  if (/^5880[0-9a-f]{256}$/.test(cbor)) return cbor.slice(4, 4 + 128);
  throw new Error(`${file}: expected cborHex 5820<32 bytes> or 5880<128 bytes>, type ${json.type}`);
}

function writePrivate(file, content) {
  fs.mkdirSync(path.dirname(file), { recursive: true, mode: 0o700 });
  fs.writeFileSync(file, content, { mode: 0o600 });
  fs.chmodSync(file, 0o600);
}

/** The stake key: the configured file, or one generated once per state dir. */
function stakeKeyHex(stateDir) {
  const configured = resolveKeyPath(process.env.FAUCET_STAKE_SKEY_FILE);
  if (configured) return privateKeyHex(configured);
  const generated = path.join(stateDir, 'faucet-stake.skey');
  if (!fs.existsSync(generated)) {
    const seed = crypto.randomBytes(32).toString('hex');
    writePrivate(
      generated,
      JSON.stringify(
        {
          type: 'StakeSigningKeyShelley_ed25519',
          description: 'Devnet test faucet stake key (generated)',
          cborHex: '5820' + seed,
        },
        null,
        2,
      ) + '\n',
    );
    console.error(`Generated a faucet stake key: ${generated}`);
  }
  return privateKeyHex(generated);
}

async function kuberSubmit(kuberUrl, txHex) {
  const res = await fetch(`${kuberUrl}/api/submit/tx`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/cbor' },
    body: Buffer.from(txHex, 'hex'),
  });
  const text = await res.text();
  if (!res.ok) throw new Error(`Kuber submit failed (${res.status}): ${text}`);
  return JSON.parse(text);
}

async function lovelaceAt(kuber, address) {
  const utxos = await kuber.queryUTxOByAddress(address);
  return utxos.reduce((sum, u) => sum + BigInt(u.txOut.value.lovelace), 0n);
}

/**
 * Moves FAUCET_FUND_ADA (default 100,000,000) from the enterprise address to
 * the base address, unless the base address already holds that much. The
 * rest stays at the enterprise address, which `cardano devnet smoke` (the
 * chain seed) spends from.
 */
async function fund(paymentHex, enterprise, base) {
  const kuberUrl = (process.env.KUBER_URL || '').replace(/\/+$/, '');
  if (!kuberUrl) throw new Error('KUBER_URL is required for --fund');
  const kuber = new KuberApiProvider(kuberUrl);

  const target = BigInt(process.env.FAUCET_FUND_ADA || '100000000') * 1000000n;
  const held = await lovelaceAt(kuber, base);
  if (held >= target) {
    console.error(`Base address already holds ${held} lovelace.`);
    return;
  }
  const available = await lovelaceAt(kuber, enterprise);
  const amount = target - held;
  if (available < amount + 10000000n) {
    throw new Error(`Enterprise address holds ${available} lovelace; cannot move ${amount}.`);
  }
  const payment = await Ed25519KeyAsync.fromPrivateKeyHex(paymentHex);
  const queryService = {
    queryUTxOByAddress: (a) => kuber.queryUTxOByAddress(a),
    queryUTxOByTxIn: (t) => kuber.queryUTxOByTxIn(t),
    queryProtocolParameters: () => kuber.queryProtocolParameters(),
    queryStakeAddressRegistered: async () => false,
  };
  const submitter = { submitTx: (hex) => kuberSubmit(kuberUrl, hex) };
  const signer = new SimpleCip30Wallet(queryService, submitter, new ShelleyWallet(payment), NETWORK_ID);

  const signed = await kuber.buildAndSignWithWallet(signer, {
    inputs: enterprise,
    outputs: [{ address: base, value: amount.toString() }],
    changeAddress: enterprise,
  });
  const txHash = await kuberSubmit(kuberUrl, signed.transaction.toBytes().toString('hex'));
  console.error(`Moving ${amount} lovelace to the base address: ${txHash}`);

  const deadline = Date.now() + Number(process.env.TX_TIMEOUT || 240000);
  while (Date.now() < deadline) {
    const utxos = await kuber.queryUTxOByTxIn(`${txHash}#0`);
    if (utxos.length > 0) {
      console.error(`Confirmed; base address holds ${await lovelaceAt(kuber, base)} lovelace.`);
      return;
    }
    await new Promise((r) => setTimeout(r, 2000));
  }
  throw new Error(`Timed out waiting for ${txHash}`);
}

async function main() {
  const stateDir = path.resolve(process.env.DEVNET_STATE_DIR || path.join(repoRoot, 'tests/devnet/.state'));
  const paymentFile = resolveKeyPath(process.env.FAUCET_SKEY_FILE);
  if (!paymentFile || !fs.existsSync(paymentFile)) {
    throw new Error(`Faucet signing key not found: ${paymentFile} (FAUCET_SKEY_FILE, DEVNET_OUTPUT_DIR)`);
  }
  const paymentHex = privateKeyHex(paymentFile);
  const stakeHex = stakeKeyHex(stateDir);

  const payment = await Ed25519KeyAsync.fromPrivateKeyHex(paymentHex);
  const stake = await Ed25519KeyAsync.fromPrivateKeyHex(stakeHex);
  const base = new ShelleyWallet(payment, stake).addressBech32(NETWORK_ID);
  const enterprise = new ShelleyWallet(payment).addressBech32(NETWORK_ID);

  const envFile = path.join(stateDir, 'faucet.env');
  writePrivate(
    envFile,
    [
      '# Generated by tests/devnet/scripts/faucet-env.js; devnet keys only.',
      `FAUCET_ADDRESS=${base}`,
      `FAUCET_PAYMENT_PRIVATE=${paymentHex}`,
      `FAUCET_STAKE_PRIVATE=${stakeHex}`,
      '',
    ].join('\n'),
  );
  console.log(`faucet base address:       ${base}`);
  console.log(`faucet enterprise address: ${enterprise}`);
  console.log(`wrote ${envFile}`);

  if (process.argv.includes('--fund')) await fund(paymentHex, enterprise, base);
}

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});
