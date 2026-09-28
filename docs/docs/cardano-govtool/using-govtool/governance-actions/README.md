# Governance Actions

Governance Actions are the core elements that make up governance on the blockchain. Anyone that has ADA in a wallet can propose a Governace Action. To submit a Governance Action, the submitter  pays a refundable deposit of 100,000 Ada. The deposit will be returned automatically back to the submitter's wallet upon completion of the Voting period.

### Every governance action will include the following:

* a deposit amount (recorded since the amount of the deposit is an updatable protocol parameter)
* a reward address to receive the deposit when it is repaid
* an anchor for any metadata that is needed to justify the action
* a hash digest value to prevent collisions with competing actions of the same type (as described earlier)

Governance Actions are covered in depth in [CIP-1694](https://www.1694.io/), which is the main defining document for governance on Cardano. You can read about the entirety of Cardano Governance in this document.&#x20;

### To Submit a Governance Action on GovTool, you will need to:

1. Connect a wallet ([compatible-wallets.md](../getting-started/compatible-wallets.md "mention")) to GovTool that has at least 100,000 Ada plus a few extra Ada for transaction fees
2. Create a "Proposal" for the Governace Action [propose-a-governance-action.md](./propose-a-governance-action.md "mention")
3. Store the metadata for your Governance Action yourself [storing-information-offline.md](../storing-information-offline.md "mention"). At this point, your Governance Action is in the Proposal stage. This is a forum for discussion and refinement, and is provided by GovTool to help users socialise and refine their proposed Actions if they so choose.
4. When you feel your Proposal is ready for a vote, Click the "Submit as a Governance Action" button. You will be asked to pay a deposit of 100,000 Ada, which will be automatically refunded to you when the Action has reached it's expiry date.

