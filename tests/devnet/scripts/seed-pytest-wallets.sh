#!/usr/bin/env bash
# Puts the backend suite's fixed wallets (tests/govtool-backend/test_data.json)
# on the devnet: each drep_wallets[i] stake key becomes a registered DRep
# (key-hash credential, anchor in the metadata bucket) and ada_holder_wallets[i]
# registers its stake key, delegates its vote to that DRep and gets funds at
# its base address (PYTEST_HOLDER_ADA, default 1000) as voting power. The suite
# reads these wallets by key hash; on a public network they exist already.
#
# Runs cardano-cli inside adaup's node container and pays from the devnet
# faucet (enterprise address). Skips wallets already set up, so it is safe to
# re-run. Called by seed.sh with .env.devnet exported.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../lib.sh"

NODE="${DEVNET_DOCKER_NETWORK}-cardano-node-1"
WORK_HOST="$DEVNET_OUTPUT_DIR/govtool-pytest"
WORK="/devnet/govtool-pytest"   # the same directory inside the node container
MAGIC=(--testnet-magic 42)
cli() { docker exec -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket "$NODE" cardano-cli conway "$@"; }

mkdir -p "$WORK_HOST"
chmod 700 "$WORK_HOST"

# Key files and CIP-119 anchors from test_data.json.
node - "$REPO_ROOT/tests/govtool-backend/test_data.json" "$WORK_HOST" <<'EOF'
const fs = require('fs');
const path = require('path');
const [file, out] = process.argv.slice(2);
const data = JSON.parse(fs.readFileSync(file, 'utf8'));
const write = (name, content) => fs.writeFileSync(path.join(out, name), content, { mode: 0o600 });
// Some fixture keys are labelled as payment keys; the bytes are what count.
const stakeKey = (key) => JSON.stringify({ ...key, type: 'StakeSigningKeyShelley_ed25519' });
data.drep_wallets.forEach((w, i) => {
  write(`drep${i}.stake.skey`, stakeKey(w['stake-skey']));
  write(`drep${i}.hash`, w['stake-vkey']);
  write(`drep${i}.jsonld`, JSON.stringify({
    '@context': { '@language': 'en-us', CIP100: 'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#', CIP119: 'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#', hashAlgorithm: 'CIP100:hashAlgorithm', body: { '@id': 'CIP119:body', '@context': { givenName: 'CIP119:givenName', objectives: 'CIP119:objectives' } } },
    hashAlgorithm: 'blake2b-256',
    authors: [],
    body: { givenName: `pytest DRep ${i}`, objectives: 'Backend integration test fixture.' },
  }));
});
data.ada_holder_wallets.forEach((w, i) => {
  write(`holder${i}.stake.skey`, stakeKey(w['stake-skey']));
  write(`holder${i}.addr`, w.address);
});
EOF

drep_count="$(ls "$WORK_HOST"/drep*.hash | wc -l | tr -d ' ')"
faucet_addr="$(cat "$DEVNET_OUTPUT_DIR/keys/faucet/payment.addr")"
pp="$(cli query protocol-parameters "${MAGIC[@]}")"
drep_deposit="$(node -e 'console.log(JSON.parse(process.argv[1]).dRepDeposit)' "$pp")"
key_deposit="$(node -e 'console.log(JSON.parse(process.argv[1]).stakeAddressDeposit)' "$pp")"

certs=()
signers=()
outs=()
for i in $(seq 0 $((drep_count - 1))); do
  hash="$(cat "$WORK_HOST/drep$i.hash")"
  cli key verification-key --signing-key-file "$WORK/drep$i.stake.skey" --verification-key-file "$WORK/drep$i.stake.vkey"
  cli key verification-key --signing-key-file "$WORK/holder$i.stake.skey" --verification-key-file "$WORK/holder$i.stake.vkey"

  if [ "$(cli query drep-state "${MAGIC[@]}" --drep-key-hash "$hash" | tr -d ' \n')" = "[]" ]; then
    name="pytest-drep$i.jsonld"
    curl -fsS -m 10 -X PUT -H 'Content-Type: text/plain' --data-binary "@$WORK_HOST/drep$i.jsonld" \
      "http://127.0.0.1:${METADATA_BUCKET_PORT}/data/$name" >/dev/null
    anchor_hash="$(docker exec "$NODE" cardano-cli hash anchor-data --file-text "$WORK/drep$i.jsonld")"
    cli governance drep registration-certificate --drep-key-hash "$hash" \
      --key-reg-deposit-amt "$drep_deposit" \
      --drep-metadata-url "$METADATA_BUCKET_URL/data/$name" --drep-metadata-hash "$anchor_hash" \
      --out-file "$WORK/drep$i.reg.cert"
    certs+=("$WORK/drep$i.reg.cert"); signers+=("$WORK/drep$i.stake.skey")
  fi

  # Funds at the holder's base address are the stake it delegates.
  pay_addr="$(cat "$WORK_HOST/holder$i.addr")"
  if [ "$(cli query utxo "${MAGIC[@]}" --address "$pay_addr" --output-json | tr -d ' \n')" = "{}" ]; then
    outs+=(--tx-out "$pay_addr+${PYTEST_HOLDER_ADA:-1000}000000")
  fi
  holder_addr="$(cli stake-address build "${MAGIC[@]}" --stake-verification-key-file "$WORK/holder$i.stake.vkey")"
  info="$(cli query stake-address-info "${MAGIC[@]}" --address "$holder_addr" | tr -d ' \n')"
  if [ "$info" = "[]" ]; then
    cli stake-address registration-certificate --stake-verification-key-file "$WORK/holder$i.stake.vkey" \
      --key-reg-deposit-amt "$key_deposit" --out-file "$WORK/holder$i.reg.cert"
    certs+=("$WORK/holder$i.reg.cert")
  fi
  case "$info" in
    *"$hash"*) ;;
    *)
      cli stake-address vote-delegation-certificate --stake-verification-key-file "$WORK/holder$i.stake.vkey" \
        --drep-key-hash "$hash" --out-file "$WORK/holder$i.deleg.cert"
      certs+=("$WORK/holder$i.deleg.cert"); signers+=("$WORK/holder$i.stake.skey")
      ;;
  esac
done

if [ "${#certs[@]}" -eq 0 ] && [ "${#outs[@]}" -eq 0 ]; then
  log "pytest wallets: already on chain"
  exit 0
fi

utxo="$(cli query utxo "${MAGIC[@]}" --address "$faucet_addr" --output-json |
  node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{const u=Object.entries(JSON.parse(s)).sort((a,b)=>b[1].value.lovelace-a[1].value.lovelace);console.log(u[0][0])})')"
cert_args=(); for c in ${certs[@]+"${certs[@]}"}; do cert_args+=(--certificate-file "$c"); done
sign_args=(--signing-key-file /devnet/keys/faucet/payment.skey); for s in ${signers[@]+"${signers[@]}"}; do sign_args+=(--signing-key-file "$s"); done
cli transaction build "${MAGIC[@]}" --tx-in "$utxo" --change-address "$faucet_addr" \
  ${cert_args[@]+"${cert_args[@]}"} ${outs[@]+"${outs[@]}"} --witness-override $((1 + ${#signers[@]})) --out-file "$WORK/tx.raw" >/dev/null
cli transaction sign "${MAGIC[@]}" --tx-body-file "$WORK/tx.raw" "${sign_args[@]}" --out-file "$WORK/tx.signed"
cli transaction submit "${MAGIC[@]}" --tx-file "$WORK/tx.signed" >/dev/null
txid="$(cli transaction txid --tx-file "$WORK/tx.signed" | grep -oE '[0-9a-f]{64}' | head -1)"
log "pytest wallets: ${#certs[@]} certificates, $((${#outs[@]} / 2)) payments in $txid"
wait_for "pytest wallets tx on chain" 120 bash -c \
  "docker exec -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket $NODE cardano-cli conway query utxo --testnet-magic 42 --tx-in '$txid#0' --output-json | grep -q '$txid'"
