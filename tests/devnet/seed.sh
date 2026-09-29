#!/usr/bin/env bash
# Seed, run by up.sh once the app stack is healthy. Safe to re-run: every
# run adds a new set of proposals.
#   1. uploads seed/bucket/* to the metadata bucket (fixture anchors the
#      Playwright invalid-metadata cases point at)
#   2. `cardano devnet smoke` round 1: DReps, delegations, committee hot keys
#      and one proposal of each DEVNET_SEED_ACTIONS type; DEVNET_SEED_RATIFY
#      get yes votes and are enacted (outcomes data), the others no votes;
#      DEVNET_SEED_EXTRA_TREASURY more treasury withdrawals are enacted too
#   3. round 2: another proposal of each type, none ratified, so every type
#      stays live for the whole run (it must follow round 1's enactment,
#      which removes live proposals of the same purpose)
#   4. scripts/seed-pytest-wallets.sh: the backend suite's fixed DReps and
#      ADA holders (tests/govtool-backend/test_data.json)
#   5. waits until the backend lists a live proposal of every type and the
#      seeded DReps have voting power (one epoch boundary after delegation)
# SKIP_CHAIN_SEED=1 does step 1 only.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
load_env

BUCKET="http://127.0.0.1:${METADATA_BUCKET_PORT}"
for file in "$DEVNET_DIR"/seed/bucket/*; do
  name="$(basename "$file")"
  curl -fsS -m 10 -X PUT -H 'Content-Type: text/plain' \
    --data-binary "@$file" "$BUCKET/data/$name" >/dev/null ||
    die "could not upload $name to the metadata bucket"
done
log "bucket seeded"

[ "${SKIP_CHAIN_SEED:-0}" = "1" ] && exit 0

log "chain seed, round 1: ratify ${DEVNET_SEED_RATIFY} and wait for enactment"
round1_actions="$DEVNET_SEED_ACTIONS"
extra=0
while [ "$extra" -lt "${DEVNET_SEED_EXTRA_TREASURY:-0}" ]; do
  round1_actions="$round1_actions,treasury"; extra=$((extra + 1))
done
"$ADAUP_CARDANO" devnet smoke --docker \
  --actions "$round1_actions" --ratify "$DEVNET_SEED_RATIFY"
cp "$DEVNET_OUTPUT_DIR/smoke/result.json" "$DEVNET_STATE_DIR/seed-enacted.json"

log "chain seed, round 2: live proposals of every type"
"$ADAUP_CARDANO" devnet smoke --docker \
  --actions "$DEVNET_SEED_ACTIONS" --ratify none --no-wait-enactment
cp "$DEVNET_OUTPUT_DIR/smoke/result.json" "$DEVNET_STATE_DIR/seed-live.json"

log "backend suite wallets (tests/govtool-backend/test_data.json)"
"$DEVNET_DIR/scripts/seed-pytest-wallets.sh"

BACKEND="http://127.0.0.1:${BACKEND_PORT}"
# Backend proposal types, from the smoke action names.
expected_types() {
  local action
  for action in ${DEVNET_SEED_ACTIONS//,/ }; do
    case "$action" in
      info) echo InfoAction ;;
      treasury) echo TreasuryWithdrawals ;;
      parameter) echo ParameterChange ;;
      hardfork) echo HardForkInitiation ;;
      no-confidence) echo NoConfidence ;;
      committee) echo NewCommittee ;;
      constitution) echo NewConstitution ;;
    esac
  done
}
live_types_ok() {
  local listed type
  listed="$(curl -fsS -m 10 "$BACKEND/proposal/list?pageSize=100" |
    node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{for(const p of JSON.parse(s).elements)console.log(p.type)})')" ||
    return 1
  for type in $(expected_types); do
    grep -qx "$type" <<< "$listed" || return 1
  done
}
drep_has_power() {
  local id
  id="$(node -e 'console.log(require(process.argv[1]).dreps.drep1.id)' "$DEVNET_STATE_DIR/seed-live.json")"
  curl -fsS -m 10 "$BACKEND/drep/list?search=$id" |
    node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{const e=JSON.parse(s).elements||[];process.exit(e.some(d=>Number(d.votingPower)>0)?0:1)})'
}
wait_for "backend listing a live proposal of every type" 300 live_types_ok
pytest_drep_has_power() {
  local id
  id="$(node -e 'console.log(require(process.argv[1]).drep_wallets[0]["drep-id"])' "$REPO_ROOT/tests/govtool-backend/test_data.json")"
  curl -fsS -m 10 "$BACKEND/drep/list?search=$id" |
    node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{const e=JSON.parse(s).elements||[];process.exit(e.some(d=>Number(d.votingPower)>0)?0:1)})'
}
wait_for "seeded DRep voting power (next epoch boundary)" 300 drep_has_power
wait_for "backend suite DRep voting power" 300 pytest_drep_has_power
