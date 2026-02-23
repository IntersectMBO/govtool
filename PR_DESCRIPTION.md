# PR: Support CIP-XXXX survey-linked Info Actions and optional survey voting metadata

## Summary
This PR adds end-to-end support in GovTool for:
1. Creating a survey payload (`surveyDetails`) via transaction metadata label `17`.
2. Linking that survey from an Info Action anchor using `kind = cardano-governance-survey-link` and `surveyRef`.
3. Voting on the linked survey from the governance vote flow by attaching `surveyResponse` metadata (also label `17`) to the vote transaction.
4. Displaying linked survey details and tally results on the Governance Action details page.

The implementation follows the current CIP-XXXX behavior decisions used for this branch:
- Survey + Info Action creation uses a **two-transaction flow**.
- Blank survey submissions are allowed: if no survey answers are provided, GovTool submits the governance vote without `surveyResponse` metadata.

## Why
Info Actions currently lack a standardized in-app way to:
- publish structured surveys,
- link them deterministically to governance actions,
- and collect responses directly in governance vote transactions.

This PR closes that gap by wiring both creation and voting paths, plus backend resolution/tally endpoints and frontend visualization.

## Backend changes

### New API endpoints
Added in `govtool/backend/src/VVA/API.hs`:
- `GET /proposal/survey/:proposalId`
- `GET /proposal/survey/:proposalId/tally?weighting=CredentialBased|StakeBased`

### New survey module
Added `govtool/backend/src/VVA/Survey.hs` with:
- SQL-backed proposal survey resolution
- SQL-backed tally response
- safe fallback payloads when no data is available

### New SQL files
Added:
- `govtool/backend/sql/get-proposal-survey.sql`
- `govtool/backend/sql/get-proposal-survey-tally.sql`

`get-proposal-survey-tally.sql` includes fail-safe handling for malformed `answers` (non-array values are treated as empty arrays instead of failing the query).

### Build/package wiring
Updated `govtool/backend/vva-be.cabal` to include:
- new SQL files under `extra-source-files`
- new exposed module `VVA.Survey`

## Frontend changes

### Survey API integration
Added request + query layer for survey endpoints:
- `govtool/frontend/src/services/requests/getProposalSurvey.ts`
- `govtool/frontend/src/services/requests/getProposalSurveyTally.ts`
- `govtool/frontend/src/hooks/queries/useGetProposalSurveyQuery.ts`
- `govtool/frontend/src/hooks/queries/useGetProposalSurveyTallyQuery.ts`

Updated exports and query keys:
- `govtool/frontend/src/services/requests/index.ts`
- `govtool/frontend/src/hooks/queries/index.ts`
- `govtool/frontend/src/consts/queryKeys.ts`

### New API models
Extended `govtool/frontend/src/models/api.ts` with survey-related types:
- `SurveyRef`, `SurveyQuestion`, `SurveyDetails`
- `ProposalSurveyResponse`, `ProposalSurveyTallyResponse`

### Governance Action details page
Updated `govtool/frontend/src/pages/GovernanceActionDetails.tsx` to:
- fetch linked survey for Info Actions,
- render validation status,
- fetch/render tally with weighting toggle (`CredentialBased` / `StakeBased`),
- display per-question method results.

### Vote flow: attach optional surveyResponse metadata
Updated:
- `govtool/frontend/src/components/molecules/VoteActionForm.tsx`
- `govtool/frontend/src/hooks/forms/useVoteActionForm.tsx`

Behavior:
- If survey link/details are valid and at least one valid answer is provided, submit vote with metadata label `17` containing `surveyResponse`.
- If no answers are provided, submit vote without `surveyResponse` metadata (blank survey allowed).
- Invalid custom JSON answer input blocks submission until corrected.

### Wallet metadata support
Updated `govtool/frontend/src/context/wallet.tsx` to support metadata auxiliary data in tx building:
- added `buildMetadataAuxiliaryData(label, payload)` helper
- extended `buildSignSubmitConwayCertTx` args to support:
  - `auxiliaryData`
  - `skipPendingCheck`
  - `skipStakeKeyRegistration`
  - `trackPending`
- builder now injects auxiliary data into tx when provided

### Info Action create flow with attached survey (two-step)
Updated:
- `govtool/frontend/src/hooks/forms/useCreateGovernanceActionForm.ts`
- `govtool/frontend/src/components/organisms/CreateGovernanceActionSteps/CreateGovernanceActionForm.tsx`
- `govtool/frontend/src/components/organisms/CreateGovernanceActionSteps/StorageInformation.tsx`
- `govtool/frontend/src/utils/survey.ts`
- `govtool/frontend/src/utils/index.ts`

Behavior:
1. User can enable `attachSurvey` and provide `surveyDetailsJson` (basic structural validation in form).
2. First submit creates a survey tx by posting metadata label `17` with `surveyDetails`.
3. `surveyHash` is computed using the current client hash utility and paired with returned survey tx id.
4. Metadata is regenerated for the Info Action anchor payload with:
   - `specVersion: "1.0.0"`
   - `kind: "cardano-governance-survey-link"`
   - `surveyRef: { surveyTxId, surveyHash }`
5. User is prompted to upload refreshed metadata URL and submit again.
6. Second submit creates the Info Action transaction.

## Behavior decisions in this PR
- **Blank survey answers are allowed**: no `surveyResponse` metadata is attached in that case.
- Survey response metadata is attached only when there is at least one valid answer.
- Survey creation path remains gated to Info Action linkage flow.

## Compatibility and risk
- Existing non-survey governance action flows remain unchanged.
- Existing voting continues to work without survey metadata.
- Main risk area is metadata hash/link correctness across toolchains; this PR keeps validation fail-safe and exposes link/validation states in the UI.

## Testing
### What was verified
- Static review of endpoint wiring, request/query hooks, and UI integration.
- `git diff --check` clean.

### Not verified in this environment
- Full TypeScript/Haskell compile and runtime integration tests could not be executed here due missing local dependencies and restricted network access for package retrieval.

Recommended CI/manual checks after merge:
1. Create survey tx (label `17`) + create Info Action link tx.
2. Vote with survey answers and confirm metadata is attached.
3. Vote without survey answers and confirm governance vote still submits without `surveyResponse` metadata.
4. Validate survey/tally endpoints against known proposals.
