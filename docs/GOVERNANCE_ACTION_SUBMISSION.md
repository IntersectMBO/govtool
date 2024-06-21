# How to submit a Governance Action using GovTool

## Prerequisites

- Follow the steps of setting up the [GovTool Frontend](../govtool/frontend/README.md).
- Provide any backend that provides the Epoch params (for the wallet connection), can be the current [GovTool Backend](../govtool/backend/README.md).
- Have a wallet with the 50k of ADA to pay for the transaction and fee.

## Development guide

### Prerequisites

For creating the Governance Action, you need to consume 2 utility methods provided by `GovernanceActionProvided` (documented later within this document), and 3 exported from `CardanoProvider` wallet actions (2 for the 2 types of supported by GovTool Governance Actions and 1 for Signing and Submitting the transaction)

### Types

```typescript
import { VotingProposalBuilder } from "@emurgo/cardano-serialization-lib-nodejs";

interface GovernanceAction {
  title: string;
  abstract: string;
  motivation: string;
  rationale: string;
  references: [{ label: string; uri: string }];
}

interface InfoProps {
  hash: string;
  url: string;
}

interface TreasuryProps {
  amount: string;
  hash: string;
  receivingAddress: string;
  url: string;
}

const createGovernanceActionJsonLD: (
  governanceAction: GovernanceAction
) => NodeObject;

const createHash: (jsonLd: NodeObject) => string;

const buildNewInfoGovernanceAction: (
  infoProps: InfoProps
) => Promise<VotingProposalBuilder | undefined>;

const buildTreasuryGovernanceAction: (
  treasuryProps: TreasuryProps
) => Promise<VotingProposalBuilder | undefined>;

const buildSignSubmitConwayCertTx: (params: {
  govActionBuilder: VotingProposalBuilder;
  type: "createGovAction";
}) => Promise<void>;
```

### Step 1: Create the Governance Action metadata object

Create the Governance Action object with the fields specified by [CIP-108](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0108), which are:

- title
- abstract
- motivation
- rationale
- references

(For the detailed documentation of the fields and their types, please refer to the [CIP-108](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0108))

### Step 2: Create the Governance Action JSON-LD

Using the `GovernanceActionProvider` provider, use the `createGovernanceActionJsonLd` method to create the JSON-LD object for the Governance Action.

Example:

```typescript
// When used within a GovernanceActionProvider
const { createGovernanceActionJsonLD } = useCreateGovernanceAction();

const jsonLd = createGovernanceActionJsonLD(governanceAction);
```

Type of the `jsonLd` object is `NodeObject` provided from the `jsonld` package by digitalbazaar ([ref](https://github.com/digitalbazaar/jsonld.js)).

### Step 3: Create the Governance Action Hash of the JSON-LD

Using the `GovernanceActionProvider` provider, use the `createGovernanceActionHash` method to create the hash of the JSON-LD object.

Example:

```typescript
// When used within a GovernanceActionProvider
const { createHash } = useCreateGovernanceAction();

const hash = createHash(jsonLd);
```

Type of the `hash` object is `string` (blake2b-256).

### Step 4: Validate the Governance Action hash and metadata

Validate the Governance Action hash and metadata using any backend that provides the Governance Action validation of the metadata against the provided hash.

### Step 5: Sign and Submit the Governance Action

Using the `CardanoProvider` provider, use the `buildSignSubmitConwayCertTx` method to sign and submit the Governance Action, and either the `buildNewInfoGovernanceAction` or `buildTreasuryGovernanceAction` method to build the transaction based on the Governance action type.

Example:

```typescript
// When used within a CardanoProvider
const { buildSignSubmitConwayCertTx, buildNewInfoGovernanceAction } =
  useCardano();

// hash of the generated Governance Action metadata, url of the metadata
const govActionBuilder = await buildNewInfoGovernanceAction({ hash, url });

// sign and submit the transaction
await buildSignSubmitConwayCertTx({
  govActionBuilder,
  type: "createGovAction",
});

// or if you want to use the Treasury Governance Action
const { buildTreasuryGovernanceAction } = useCardano();

// hash of the generated Governance Action metadata, url of the metadata, amount of the transaction, receiving address is the stake key address
const govActionBuilder = await buildTreasuryGovernanceAction({
  hash,
  url,
  amount,
  receivingAddress,
});

// sign and submit the transaction
await buildSignSubmitConwayCertTx({
  govActionBuilder,
  type: "createGovAction",
});
```

### Step 6: Verify the Governance Action

`buildSignSubmitConwayCertTx` logs the transaction CBOR making it able to be tracked on the transactions tools such as cexplorer.

## Additional steps for using the GovTool metadata validation on the imported Pillar component

```tsx
enum MetadataValidationStatus {
  URL_NOT_FOUND = "URL_NOT_FOUND",
  INVALID_JSONLD = "INVALID_JSONLD",
  INVALID_HASH = "INVALID_HASH",
  INCORRECT_FORMAT = "INCORRECT_FORMAT",
}
// Using the props passed to the component
type Props = {
  validateMetadata: ({
    url,
    hash,
    standard,
  }: {
    url: string;
    hash: string;
    standard: "CIP108";
  }) => Promise<{
    metadata?: any;
    status?: MetadataValidationStatus;
    valid: boolean;
  }>;
};

import React, { Suspense } from "react";

const SomeImportedPillar: React.FC<Props> = React.lazy(
  () => import("path/to/SomeImportedPillar")
);

const SomeWrapperComponent = () => {
  const { validateMetadata } = useValidateMutation();

  return (
    <Suspense fallback={<div>I am lazy loading...</div>}>
      <SomeImportedPillar validateMetadata={validateMetadata} />
    </Suspense>
  );
};
```
