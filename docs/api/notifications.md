# Notifications API (planned)

Status: planned. No backend serves it yet. The frontend already calls
`GET /notifications/config` and, while that does not answer with a usable
config, works out notifications in the browser from the routes it already
reads ("local mode"). A service that implements this contract switches every
user to it with no frontend change ("remote mode").

The paths are relative to the frontend's `VITE_BASE_URL`, like every other
backend route. The service may live elsewhere (for example beside the
newsletter's mail sender) as long as the gateway routes `/notifications` to it.

CORS: the frontend sends `Authorization` and `Content-Type` only, with GET,
POST and PUT. Of the response headers it reads only `Retry-After`.

Errors use the backend's legacy body, `{ "errorType": "…", "message": "…" }`:
400 for a malformed request, 401 for a missing, expired or revoked token (the
frontend then asks the user to sign again), 404 for an unknown route, 429 with
`Retry-After` when rate limited.

## Discovery

### GET /notifications/config

No auth. Anything other than a 200 with a usable body keeps the frontend in
local mode.

```json
{
  "version": 1,
  "compatible": [1],
  "channels": { "inApp": true, "email": true },
  "events": ["drepChange", "delegationChange", "actionActivity", "relevantAction", "digest", "system"],
  "frequencies": ["immediate", "daily", "weekly", "off"],
  "emailSetupUrl": "https://…"
}
```

- `version`: the service's contract version. A frontend that speaks version 1
  accepts `version: 1`, or a later version whose `compatible` lists 1. Changes
  that only add fields or kinds keep `version` as it is.
- `channels.email: false` disables email in the preferences page.
- An event missing from `events` is hidden there.
- `emailSetupUrl` (optional, `https://` only): where a user adds or confirms an
  address. The preferences page links to it when email is on and there is no
  address. GovTool has no address field of its own; the address may come from
  the newsletter subscription or this page.

## Sign-in

The frontend proves wallet ownership with CIP-30 `signData`, then keeps the
token in `sessionStorage` (per stake address) and sends it as
`Authorization: Bearer <token>`.

### POST /notifications/auth/nonce

```json
{ "stakeAddress": "stake1…" }
```
→ `{ "nonce": "<string to sign>", "expiresAt": "<ISO-8601>" }`

The nonce is single use, bound to that stake address, and expires within five
minutes. Rate limit this route per address and per client.

### POST /notifications/auth

```json
{ "stakeAddress": "stake1…", "nonce": "…", "signature": "<COSE_Sign1 hex>", "key": "<COSE_Key hex>" }
```
→ `{ "token": "…", "expiresAt": "<ISO-8601>" }`

What the frontend signs: `signData(<reward address hex>, <hex of the nonce's
UTF-8 bytes>)`. The service checks, per CIP-8/CIP-30:

- the payload of the COSE_Sign1 is exactly the nonce it issued, unused and
  unexpired;
- the protected header's `address` is the reward address of `stakeAddress`,
  and its network matches the network the service serves;
- the key in COSE_Key verifies the signature and hashes to the stake key
  credential of that address. A script stake credential cannot sign and is
  refused with 400.

Tokens should last no longer than a day. There is no logout call; the token
dies with the browser tab's session storage or at `expiresAt`.

## Feed

### GET /notifications?page=1&pageSize=7&filter=all&sort=newest

`page` is 1-based; `pageSize` at most 50. `sort`: `newest`, `oldest`,
ordered by `createdAt` and then `id`, so a page boundary is stable.
`filter`, with the kinds each one covers (they overlap, as the design shows):

| filter | kinds |
|---|---|
| `all` | every kind |
| `unread` | every kind, `read: false` only |
| `governanceActions` | `actionActivity`, `relevantAction`, `digest` |
| `delegation` | `delegationChange` |
| `drep` | `drepChange` |
| `myActivity` | `actionActivity`, `delegationChange` |

`system` appears only under `all` and `unread`.

```json
{
  "items": [Notification],
  "total": 18,
  "unread": 4,
  "counts": { "all": 18, "unread": 4, "governanceActions": 9, "delegation": 3, "drep": 4, "myActivity": 6 }
}
```

`total` counts the requested filter. `unread` and `counts` are for the whole
feed, whatever the filter. Items that arrive while a user pages can repeat
across pages; the frontend drops repeated ids.

A `Notification` is structured data; the frontend writes the sentence (copy
and translations stay in one place).

```json
{
  "id": "stable-id",
  "kind": "drepChange",
  "createdAt": "2026-10-02T15:24:00Z",
  "read": false,
  "data": { }
}
```

Amounts are lovelace as decimal strings, as elsewhere in the backend.

| kind | data |
|---|---|
| `drepChange` | `{ "dRepId": "drep1…", "dRepName": "Hazel Pool" \| null, "change": "retired" \| "inactive" \| "retiring" \| "metadataUpdated", "epoch": 604 \| null, "votingPower": "1840000000000" \| null }` |
| `delegationChange` | `{ "change": "delegated" \| "removed", "target": { "type": "drep" \| "abstain" \| "noConfidence", "dRepId": "drep1…" \| null, "dRepName": "…" \| null }, "votingPower": "1840000000000" \| null }` |
| `actionActivity` | `{ "actionId": "<txHash>#<index>", "actionType": "TreasuryWithdrawals", "title": "…" \| null, "outcome": "ratified" \| "enacted" \| "expired", "vote": "yes" \| "no" \| "abstain" \| null, "relation": "voted" \| "dRepVoted" }` |
| `relevantAction` | `{ "actionId": "<txHash>#<index>", "actionType": "ParameterChange", "title": "…" \| null, "expiryDate": "<ISO-8601>" \| null, "reason": "deadline" \| "matched" }` |
| `digest` | `{ "from": "<ISO-8601>", "to": "<ISO-8601>", "total": 14, "relevant": 2 \| null, "url": "https://…" \| null }` |
| `system` | `{ "title": "…", "body": "…", "url": "https://…" \| null }` (the only kind whose text comes from the service) |

The frontend leaves out an item of an unknown `kind`, or one missing a field
the screen reads, so new kinds can ship on the service first. It opens a
`url` only when it starts with `http://` or `https://`.

### POST /notifications/read

`{ "ids": ["…"], "read": true }` → 204. `read: false` marks unread.

### POST /notifications/read-all

→ 204.

## Preferences

### GET /notifications/preferences · PUT /notifications/preferences

```json
{
  "channels": {
    "inApp": true,
    "email": { "enabled": false, "address": "ada@mail.com", "status": "verified" }
  },
  "events": {
    "drepChange": "immediate",
    "delegationChange": "immediate",
    "actionActivity": "immediate",
    "relevantAction": "immediate",
    "digest": "weekly"
  }
}
```

- PUT takes the same body and answers with what was stored. The frontend
  sends `email.address` back unchanged; changing the address happens at
  `emailSetupUrl`.
- `email.status`: `unverified`, `pending` (confirmation sent) or `verified`.
  Mail goes only to a verified address.
- Frequencies: in-app shows an event unless it is `off`; email sends it at once
  (`immediate`) or in a daily or weekly digest. The preferences page offers
  on/off today (on is `immediate`, `weekly` for `digest`); other values a
  service stores are kept as they are.

## Personal data

An email address tied to a stake address is personal data. Keep it only while
email is enabled or a confirmation is pending, delete it when the user turns
email off or removes it, and say so in the privacy policy before launch.

## What local mode does instead

The frontend derives `drepChange` (retired or inactive only),
`delegationChange` (one per delegation transaction, against a stored
snapshot), `actionActivity` (actions voted on, decided in the past 30 days;
those decided before the browser's first visit arrive read),
`relevantAction` (for DReps and direct voters, unvoted actions closing within
7 days) and a weekly `digest` of new actions. It keeps read state and
preferences in `localStorage` per stake address, and offers in-app only. See
`govtool/frontend/src/utils/notifications.ts`.
