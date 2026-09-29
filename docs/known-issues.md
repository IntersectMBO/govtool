# Known issues: proposal discussion forum (pdf) and its tests

This document lists the GitHub issues that relate to the pdf pillar, to its Playwright tests and to the pdf test
infrastructure. For each one it records what fixed it, what remains, and any open questions.

**Sources**
- IntersectMBO/govtool issues: all open issues, plus those closed since 2026-03-20.
- IntersectMBO/govtool-proposal-pillar has issues disabled.
- cardanoapi/govtool issues mirror the revamp epics.
- The 2026-09-26 Playwright run: `tests/govtool-frontend/playwright/reports/pdf-backend-run-2026-09-26.md`, defects D1–D10.
- The rebuilt backend: `govtool/govtool-pdf-backend/SPEC.md`, with deviations Δ1–Δ47 in §13.

**Status words**
- **Fixed (rebuild):** the new NestJS backend removes the cause.
- **Fixed (this change):** fixed in the frontend's vendored pdf-ui or in the tests on this branch.
- **Open:** still to do.
- **Question:** a decision is needed.
- Nothing is merged or closed on GitHub yet. "Fixed" means fixed on this branch.

## 1. Issues that match a Playwright defect

| Issue | Title (short) | Defect | Status | How |
|---|---|---|---|---|
| [#4116](https://github.com/IntersectMBO/govtool/issues/4116) | 7E_1–4: more than 7 links allowed | D3 | Fixed (this change) | Per the maintainer (and #4087) there is no link limit. pdf-ui drops the "(up to 7 entries)" copy and the unused `maxLinks` cap (`LinkManager.jsx`, `Step2.jsx`, `EditProposalDialog`). 7E now asserts that an 8th link can be added. |
| [#4087](https://github.com/IntersectMBO/govtool/issues/4087) | Increase reference-link limit | D3 | Closed upstream | Root cause of the 7E test drift; no action needed. |
| [#4000](https://github.com/IntersectMBO/govtool/issues/4000) | Treasury amount in ADA | D4 | Fixed (this change) | The ADA change is what added "₳ 929". Amount assertions (7D, 7M_2, budget costing) now accept the value with or without "₳ " (`lib/helpers/adaFormat.ts` `adaAmountText()`). |
| [#3602](https://github.com/IntersectMBO/govtool/issues/3602) | Insufficient balance despite funds | D8 | Fixed (this change) | The hard-coded 100000.18 ADA gate is now `epochParams.gov_action_deposit` + 0.18 ADA (`SingleGovernanceAction/index.jsx`). The dialog shows the real deposit. |
| [#4125](https://github.com/IntersectMBO/govtool/issues/4125) | 150K tADA still "not enough balance" | D8 + balance parsing | Fixed (this change), to be confirmed by reporter | Two causes found. pdf-ui parsed CIP-30 `getBalance` with a regex that only accepts an 8-byte CBOR integer, so any wallet holding tokens (or with less than about 4295 ADA) read as **0**. It now uses CSL `Value.from_hex().coin()`, with unit tests. The maintainer's rewards hypothesis is untested. |
| [#4174](https://github.com/IntersectMBO/govtool/issues/4174) | >100K ADA, still insufficient | D8 + balance parsing | Closed as duplicate of #4125 | Same fix. |
| [#4071](https://github.com/IntersectMBO/govtool/issues/4071) | Insufficient balance (Brave + Eternl) | balance parsing | Closed (cannot reproduce) | Plausibly the token-holding wallet case above: Eternl returns `[coin, multiasset]`. |
| [#1981](https://github.com/IntersectMBO/govtool/issues/1981) | Remember login in browser | D6 | Open | pdf-ui clears a valid session on reload while `wallet.stakeKey` is still unset (`lib/helpers.js:28-34`). Not fixed yet; see §4. |
| [#3796](https://github.com/IntersectMBO/govtool/issues/3796) | Refresh removes BD voting section | D6, D9 | Open | Same cause as #1981, plus the stale wallet snapshot (D9). |
| [#4078](https://github.com/IntersectMBO/govtool/issues/4078) | Can't verify as DRep | D2, D9 | Open | Sign-in races with the wallet context. |
| [#4162](https://github.com/IntersectMBO/govtool/issues/4162) | "Verify yourself" loop | D2 | Closed (not reproducible) | Matches `challenge?identifier=undefined`, which the run reproduces. |
| [#4049](https://github.com/IntersectMBO/govtool/issues/4049) | Signing too many times | D2, D6 | Closed (not reproducible) | Same family. |
| [#2986](https://github.com/IntersectMBO/govtool/issues/2986) | Constitution submission error handling | D5-adjacent | Open | Constitution URL fetch goes through the backend proxy (Δ45). Guardrails pre-validation and error UX are not addressed. |
| [#3917](https://github.com/IntersectMBO/govtool/issues/3917) | No Confidence GA cannot be submitted (7H_4) | D11 | Open, root cause found | The node rejects the tx with `InvalidPrevGovActionId (… NoConfidence SNothing …)`: preview already has an enacted committee action, so a No Confidence needs the previous action id. pdf-ui calls `buildNoConfidenceGovernanceAction({hash,url})` without it (`InformationStorageStep.jsx:169-175`), and GovTool's builder can't accept one (`NoConfidenceProps = VotingAnchor`, `context/wallet.tsx:127`; `NoConfidenceAction.new()`, `:1233-1260`). Fix spans both: GovTool's builder takes the previous action id (as the hard-fork builder does), and pdf-ui fetches it (via the govtool proxy's `enacted-details`, as for hard fork) and passes it. |
| [#3949](https://github.com/IntersectMBO/govtool/issues/3949) | `e.t0.includes is not a function` | D12 | Open, root cause found | The submit catch block calls `error?.includes(...)` on an Error object (`InformationStorageStep.jsx:224-228`). That throws inside the catch and hides every submission failure, which is why #3917 shows no error. Fix in pdf-ui: read `error?.message ?? String(error)`. |

## 2. Issues fixed by the backend rebuild

| Issue | Title (short) | Status | How |
|---|---|---|---|
| [#4168](https://github.com/IntersectMBO/govtool/issues/4168) / [#4179](https://github.com/IntersectMBO/govtool/issues/4179) | Open SSRF proxy `POST /api/proxy` | Fixed (rebuild) | Δ45. The proxy needs a login and allows GET only, http/https/ipfs only. Private, loopback and metadata ranges are blocked, with the resolved IP pinned per connection and every redirect re-checked. Responses are limited to 10 s and 5 MiB. The constitution-hash flow still works, which was the objection to the removal PRs. |
| [#4159](https://github.com/IntersectMBO/govtool/issues/4159) | Admin credentials and budget data exposed via Strapi public role | Fixed (rebuild) | There is no admin panel, and there are no routes for the BD section collections. Users are only ever returned as `{id, govtool_username}` (Δ1, Δ2, Δ36, Δ39). |
| [#4172](https://github.com/IntersectMBO/govtool/issues/4172) | Auth challenges publicly listable | Fixed (rebuild) | The only challenge route is `GET /api/auth/challenge`. Challenges are single-use, bound to the signed payload and purged on expiry (Δ14–Δ16). |
| [#3715](https://github.com/IntersectMBO/govtool/issues/3715) | Invalid links accepted (UI + API) | Partly fixed | API half: Δ47, links must be http/https/ipfs or the request gets a 400. UI half (the error clears on whitespace edits) is open. |
| [#4170](https://github.com/IntersectMBO/govtool/issues/4170) | PII and DRep vote leak | Partly fixed | Contact information and emails are never returned (Δ11), and users are projected. **Question:** `bd-poll-votes` rows with `drep_id` and `vote_result` stay public because pdf-ui's DRep voters dialog reads them. Should they be restricted? |
| [#3739](https://github.com/IntersectMBO/govtool/issues/3739) | Sorting by most likes wrong | Likely fixed | Deep sort with a tie-breaker (§4.4), and counters kept atomically (Δ9, Δ19). 8B_2 passes. |
| [#3689](https://github.com/IntersectMBO/govtool/issues/3689) | Refresh token expires too early | Largely fixed | The refresh token is a 7-day httpOnly JWT, reissued on refresh (Δ17, Δ18). pdf-ui only refreshes when a username is set (SPEC §12.12); that part is open. |
| [#3979](https://github.com/IntersectMBO/govtool/issues/3979) | IPFS metadata URL unsupported | Kept working | The proxy rewrites `ipfs://` to `IPFS_GATEWAY_URL`. |
| [#2684](https://github.com/IntersectMBO/govtool/issues/2684) | Failed to fetch pdf-ui module app.cjs | Likely fixed | pdf-ui is now vendored into the frontend build (D139). There is no separately published bundle. |

## 3. Test-infrastructure issues

| Issue / defect | Status | How |
|---|---|---|
| D1 (no issue): new HD wallets have no pdf username, so the "setup your username" prompt blocked most logged-in specs | Fixed (this change) | One helper, `setUsernameIfPrompted()` (`lib/pages/pdfUsername.ts`), runs after every pdf sign-in: the submission-page `goto()`s, `verifyIdentity()` on the four discussion pages, and the ga spec. 6I–6L still assert the prompt itself. The new **6P** covers first login on budget discussion. |
| D7 (no issue): the page wallet had no CIP-30 `getBalance` | Fixed (this change) | `pageWallet.ts` exposes `getBalance` as hex CBOR of `SimpleCip30Wallet.getBalance()`. **Upstream:** libcardano-wallet's `Cip30Wrapper` (`toProtableCip30()`) leaves out `getBalance`, although the wallet has it. Needs a fix in the library, in ~/Documents/cardano-node-js; not edited. |
| D10 (no issue): 7J_1 afterEach crashes when beforeEach failed | Open | Not in scope of this change. |
| D5 (no issue): pdf-ui rejects URLs with a port, so the local bucket `127.0.0.1:3001` was invalid | Fixed (this change) | pdf-ui has a test mode. GovTool passes `allowUrlPorts` when `VITE_APP_ENV` is `development` or `test`, and then every pdf-ui URL check accepts `:1`–`:65535`. Production is unchanged. **Question:** should ports be accepted everywhere? A URL with a port is a valid anchor. |
| [#4232](https://github.com/IntersectMBO/govtool/issues/4232) | Open | Integration-test rewrite epic; the natural parent for D1, D7 and D10. |
| [#1118](https://github.com/IntersectMBO/govtool/issues/1118) | Closed (superseded by #4232) | Its pdf items were 7P (now D7/D8, fixed), 8B_2 (#3739) and the 11M flake (#2791). |
| [#2791](https://github.com/IntersectMBO/govtool/issues/2791) | Open | db-sync delay on DRep updates, a secondary factor in D9 (11K). |
| [#3336](https://github.com/IntersectMBO/govtool/issues/3336) | Open | Footer Help link, a candidate for the untriaged mobile 6M failure (not pdf). |

## 4. pdf defects: status

- **D2 — sign-in before the stake key is set.** **Fixed (D144, P1).** GovTool passes `walletStatus` (`disconnected | connecting | ready`); pdf-ui gets no `walletAPI` until `ready`, and no challenge is sent without a stake key. 7D_3, 7I_1, 7I_4, 7M_1, 8G, 12D_3, 12F_5 and 12H pass (reports/pdf-ui-alignment-2026-09-28.md).
- **D6 — session cleared on reload** (#1981, #3796). **Fixed (D144, P1).** The session is kept while `connecting` and only checked once `ready`; one session-sync effect replaces the three clearing paths. 12J passes.
- **D9 — stale wallet snapshot** (#3796, #4078). **Fixed (D144, P1).** pdf-ui reads `walletAPI` and the DRep status live from props; late voter info shows the DRep link (unit-tested). 11K, 11L and 11M pass. The host-side cache half (voter info kept across a wallet switch) was fixed in `useGetVoterInfoQuery`/`wallet.tsx` by the other session.
- **D10 — 7J_1 afterEach crash.** Did not occur in the last run; still unguarded.
- **D11 — No Confidence without the previous action id** (#3917). **Fixed (D147).** pdf-ui asks GovTool for the last enacted action of the lineage (`GET /proposal/enacted-details`, which now answers the committee and constitution lineages) and names it for Motions of No Confidence and Updates to the Constitution; the wallet's no-confidence builder takes it. To be confirmed by 7H_3 and 7H_4 on the devnet, whose seed enacts a committee and a constitution.
- **D12 — the submit catch block throws** (#3949). **Fixed (D147):** the insufficient-balance check reads `error.message`.
- **D13 — environment: the constitution URL is not reachable from the backend.** 7H_3's constitution file is at `http://127.0.0.1:3001/...` (the local bucket). `POST /api/proxy` rightly refuses it, 400 "Destination not allowed", which is the #4168 SSRF guard. Inside the container `127.0.0.1` is not the host anyway. **Question:** for local runs, set `PDF_ALLOW_PRIVATE_URLS=true` on the pdf backend and make the bucket reachable from the container, e.g. via `host.docker.internal`? That needs the test's bucket URL to be one both the browser and the container resolve.
- **D14 — "Go to Data Edit Screen" doesn't open the edit form** (7P; pdf-ui, since 2024-09). The button only closes the submission dialog (`InformationStorageStep.jsx:550` → `SingleGovernanceAction/index.jsx:2527`). Related closed issue #2252. **Question:** should it open the edit dialog, as the test expects, or is closing it the intended behaviour, in which case the test changes?
- **D15 — test cleanup after reload.** **Fixed** in the fixtures (cleanups use `verifyIdentity()`); 7J_1, 7J_2 and 7K pass.
- **Budget links** still stop at "(maximum of 20 entries)". **Question:** is the no-limit decision in #4087 meant to cover budget discussions too?
- **Upstream:** libcardano-wallet `Cip30Wrapper` lacks `getBalance` (see D7).

## 5. Verification

- The frontend `tsc`, `lint`, `test` (357 tests, including the new balance-parser tests) and `build` all pass.
- The backend `verify` (396 unit tests) and `test:e2e` (243 tests) pass.
- Playwright on fresh wallets with no seeded usernames: see the run result below.

### Playwright, fresh wallets (`HD_RUN_ID=pdfrun0926c`, no seeded usernames)

| Set | Before (2026-09-26 run) | After these fixes |
|---|---|---|
| pdf projects at 4 workers | 40 / 79 | 63 / 79 |
| after re-running the failures once serially | 53 / 79 | **72 / 79** |
| 6I–6L, plus the new 6P | 4 / 4 (6P did not exist) | 5 / 5 |

- **Now passing:** 7E_1–4 (D3), 7M_2 (D4), 7G_3, 7I_3 and 8P (D5), 7K and 7O. 7H_1 and 7H_2 now submit info and treasury governance actions on chain (D7, D8).
- **Still failing (7):**

| Test | Cause |
|---|---|
| 12H | D2 |
| 12J | D6 |
| 7H_3 | D13 (then possibly D11) |
| 7H_4 | D11, hidden by D12 |
| 7P | D14, with cleanup D15 |
| 7J_1 | D15 via D6 (the test body passed) |
| 7J_2 | D15 via D6 (the test body passed) |

- **Backend:** no failure in either run was caused by govtool-pdf-backend.


### After the pdf-ui alignment (2026-09-28, `HD_RUN_ID=pdfui0928a`, :8090 build of the final tree)

| Set | Result |
|---|---|
| pdf projects | **76 / 79** |
| 6I–6L, plus 6P | 5 / 5 |
| Mobile pdf specs | 21 / 21 |

- **Still failing:** 7H_3 (D13, environment), 7H_4 (D11) and 7P (D14). 7H_4 and 7P also fail in CI #491.
- **Also fixed in this round:**
  - 8B_1, a regression from the restyle, fixed during the run;
  - the budget draft modal's `close-button` threw a ReferenceError;
  - GovTool's Field/atom inputs dropped `onBlur`;
  - concurrent Playwright runs clobbered `lib/_mock/sharedDReps.json`.
- **Details:** `tests/govtool-frontend/playwright/reports/pdf-ui-alignment-2026-09-28.md`.
