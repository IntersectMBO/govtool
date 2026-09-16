import { expect, Page } from "@playwright/test";
import { Decoder } from "cbor-x";
import { blake2b } from "blakejs";
// Use the frontend's pinned codecs, without adding a second dependency version.
import * as CSL from "../../../../../govtool/frontend/node_modules/@emurgo/cardano-serialization-lib-asmjs/cardano_serialization_lib.js";
import {
  encodeMetadata,
  type SurveyDefinition,
  type Question,
} from "../../../../../govtool/frontend/node_modules/cip-179/dist/index.js";
import { metadatumCodec } from "../../../../../govtool/frontend/src/cip179/csl";

export { CSL };
export const actionHash = "44".repeat(32);
export const surveyHash = "ab".repeat(32);
// Public test key and nonexistent UTxO. Never fund this address.
const key = CSL.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
const credential = CSL.Credential.from_keyhash(key.to_public().hash());
const address = CSL.BaseAddress.new(0, credential, credential).to_address();
const stakeAddress = CSL.RewardAddress.new(0, credential).to_address();
const utxo = CSL.TransactionUnspentOutput.new(
  CSL.TransactionInput.new(CSL.TransactionHash.from_hex("33".repeat(32)), 0),
  CSL.TransactionOutput.new(
    address,
    CSL.Value.new(CSL.BigNum.from_str("5000000000"))
  )
);
export const questions: Question[] = [
  {
    type: "singleChoice",
    prompt: "Choose a priority",
    required: true,
    options: { type: "options", labels: ["Usability", "Documentation"] },
  },
  {
    type: "numericRange",
    prompt: "Choose a percentage",
    constraints: { min: 0n, max: 50n, step: 1n },
  },
];
export const definition = (
  overrides: Partial<SurveyDefinition> = {}
): SurveyDefinition => ({
  specVersion: 5,
  title: "Governance survey",
  description: "Help improve governance.",
  owner: { type: "key", keyHash: new Uint8Array(28) },
  eligibleRoles: [0],
  endEpoch: 530,
  submissionMode: { type: "public" },
  questions,
  ...overrides,
});
export const envelope = (survey = definition()) => ({
  txId: surveyHash,
  surveyIndex: 0,
  metadataLabel: 17,
  payloadCborHex: Buffer.from(
    metadatumCodec.metadatumToCbor(
      encodeMetadata({ type: "definitions", definitions: [survey] })
    )
  ).toString("hex"),
});
const params = {
  min_fee_a: 44,
  min_fee_b: 155381,
  pool_deposit: 500000000,
  key_deposit: 2000000,
  coins_per_utxo_size: 4310,
  max_val_size: 5000,
  max_tx_size: 16384,
  protocol_major: 10,
  epoch_no: 500,
  gov_action_lifetime: 30,
  gov_action_deposit: 1000000000,
};

export async function setup(
  page: Page,
  options: {
    survey?: SurveyDefinition;
    enabled?: boolean | "default";
    linked?: boolean;
    registered?: boolean;
    connected?: boolean;
    rejectSigning?: boolean;
    envelope?: Record<string, unknown>;
    surveyStatus?: number;
    presentation?: { body: string; status?: number };
  } = {}
) {
  const transactions = { unsigned: [] as string[], signed: [] as string[] };
  const requests: string[] = [];
  const errors: string[] = [];
  page.on("pageerror", (error) => errors.push(error.message));
  await page.exposeFunction("fixtureSign", (hex: string, partial: boolean) => {
    transactions.unsigned.push(hex);
    expect(partial).toBe(true);
    if (options.rejectSigning) throw new Error("User declined signing");
    const hash = CSL.FixedTransaction.from_hex(hex).transaction_hash();
    const witnesses = CSL.TransactionWitnessSet.new();
    const vkeys = CSL.Vkeywitnesses.new();
    vkeys.add(CSL.make_vkey_witness(hash, key));
    witnesses.set_vkeys(vkeys);
    return witnesses.to_hex();
  });
  await page.exposeFunction("fixtureSubmit", (hex: string) => {
    transactions.signed.push(hex);
    return CSL.FixedTransaction.from_hex(hex).transaction_hash().to_hex();
  });
  await page.addInitScript(
    ({
      enabled,
      connected,
      addressHex,
      stakeHex,
      publicKey,
      utxoHex,
      params,
    }) => {
      const w = window as any;
      w.__ENV__ = {
        VITE_BASE_URL: `${location.origin}/fixture-api`,
        VITE_METADATA_API_URL: `${location.origin}/fixture-metadata`,
        VITE_NETWORK_FLAG: "0",
        VITE_IS_DEV: "true",
        ...(enabled === "default"
          ? {}
          : { VITE_IS_CIP179_ENABLED: String(enabled) }),
        VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED: "false",
        VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED: "false",
      };
      localStorage.setItem("protocol_params", JSON.stringify(params));
      if (!connected) return;
      localStorage.setItem("wallet_data_name", JSON.stringify("fixture"));
      localStorage.setItem("wallet_data_stake_key", JSON.stringify(stakeHex));
      localStorage.setItem("network_info_fixture", "true");
      w.cardano = {
        fixture: {
          name: "Fixture wallet",
          apiVersion: "1.0.0",
          supportedExtensions: [{ cip: 95 }],
          isEnabled: async () => true,
          enable: async () => ({
            getChangeAddress: async () => addressHex,
            getUsedAddresses: async () => [addressHex],
            getUnusedAddresses: async () => [],
            getRewardAddresses: async () => [stakeHex],
            getExtensions: async () => [{ cip: 95 }],
            getNetworkId: async () => 0,
            getUtxos: async () => [utxoHex],
            getBalance: async () => "1b000000012a05f200",
            cip95: {
              getPubDRepKey: async () => publicKey,
              getRegisteredPubStakeKeys: async () => [publicKey],
              getUnregisteredPubStakeKeys: async () => [],
            },
            signTx: w.fixtureSign,
            submitTx: w.fixtureSubmit,
          }),
        },
      };
    },
    {
      enabled: options.enabled ?? true,
      connected: options.connected ?? true,
      addressHex: address.to_hex(),
      stakeHex: stakeAddress.to_hex(),
      publicKey: key.to_public().to_hex(),
      utxoHex: utxo.to_hex(),
      params,
    }
  );

  const proposal = {
    id: `${actionHash}#0`,
    txHash: actionHash,
    index: 0,
    type: "InfoAction",
    title: "Survey-linked Info Action",
    abstract: "A test governance action.",
    motivation: "Improve governance.",
    rationale: "Gather feedback.",
    createdDate: "2026-01-01T00:00:00Z",
    createdEpochNo: 500,
    expiryDate: "2099-01-01T00:00:00Z",
    expiryEpochNo: 531,
    metadataHash: "66".repeat(32),
    url: "https://fixture.invalid/action.jsonld",
    protocolParams: null,
    references: [],
    dRepYesVotes: 0,
    dRepNoVotes: 0,
    dRepAbstainVotes: 0,
    ccYesVotes: 0,
    ccNoVotes: 0,
    ccAbstainVotes: 0,
    poolYesVotes: 0,
    poolNoVotes: 0,
    poolAbstainVotes: 0,
    json: {
      body:
        options.linked === false
          ? {}
          : {
              cip179: {
                specVersion: 5,
                kind: "survey-link",
                surveyTxId: surveyHash,
                surveyIndex: 0,
              },
            },
    },
  };
  await page.route("**/*", async (route) => {
    const url = new URL(route.request().url());
    if (
      url.href === "https://fixture.invalid/presentation.json" &&
      options.presentation
    ) {
      return route.fulfill({
        contentType: "application/json",
        ...options.presentation,
      });
    }
    // No test is allowed to reach a real API, wallet backend or third party.
    if (url.origin !== "http://127.0.0.1:4179") return route.abort();
    if (!url.pathname.startsWith("/fixture-")) return route.continue();
    requests.push(url.pathname);
    let json: unknown;
    let status = 200;
    if (url.pathname === "/fixture-api/epoch/params") json = params;
    else if (url.pathname === "/fixture-api/network/info")
      json = {
        networkName: "preview",
        currentEpoch: 500,
        currentTime: "2026-01-01T00:00:00Z",
        currentBlock: 1,
      };
    else if (url.pathname.startsWith("/fixture-api/drep/info/"))
      json = {
        isRegisteredAsDRep: options.registered ?? true,
        isRegisteredAsSoleVoter: false,
        votingPower: 5000000000,
        givenName: "Test DRep",
      };
    else if (url.pathname.startsWith("/fixture-api/account/"))
      json = {
        isRegistered: true,
        isScriptBased: false,
        view: stakeAddress.to_bech32(),
        id: 1,
      };
    else if (url.pathname.startsWith("/fixture-api/proposal/get/"))
      json = { proposal, vote: null };
    else if (url.pathname.startsWith("/fixture-api/proposal/list"))
      json = { elements: [], total: 0, page: 0, pageSize: 10 };
    else if (url.pathname.startsWith("/fixture-api/survey/definition/")) {
      json = options.envelope ?? envelope(options.survey);
      status = options.surveyStatus ?? 200;
    } else if (url.pathname === "/fixture-metadata/validate")
      json = { valid: true, status: null, metadata: proposal };
    else if (url.pathname === "/fixture-api/network/total-stake")
      json = {
        totalStakeControlledByDReps: 1,
        totalStakeControlledBySPOs: 1,
        alwaysAbstainVotingPower: 0,
        alwaysNoConfidenceVotingPower: 0,
      };
    else if (url.pathname === "/fixture-api/network/metrics")
      json = {
        noOfCommitteeMembers: 3,
        quorumNumerator: 2,
        quorumDenominator: 3,
      };
    else if (url.pathname.startsWith("/fixture-api/transaction/"))
      json = { transactionConfirmed: false, votingProcedure: [] };
    else json = null;
    await route.fulfill({ status, json });
  });
  return { transactions, requests, errors };
}

export async function openVote(page: Page) {
  await page.goto(`/connected/governance_actions/${actionHash}#0`);
  await expect(page.getByTestId("yes-radio")).toBeVisible();
  await page.getByTestId("yes-radio").click();
}

export async function submitVote(page: Page) {
  await page.getByTestId("vote-button").click();
  await page.getByTestId("confirm-modal-button").click();
}

export function checkTransaction(transactions: {
  unsigned: string[];
  signed: string[];
}) {
  expect(transactions.unsigned).toHaveLength(1);
  expect(transactions.signed).toHaveLength(1);
  const unsigned = CSL.Transaction.from_hex(transactions.unsigned[0]);
  const signed = CSL.Transaction.from_hex(transactions.signed[0]);
  expect(signed.body().to_hex()).toBe(unsigned.body().to_hex());
  expect(signed.auxiliary_data()?.to_hex()).toBe(
    unsigned.auxiliary_data()?.to_hex()
  );
  const aux = signed.auxiliary_data();
  expect(signed.body().auxiliary_data_hash()?.to_hex()).toBe(
    aux ? CSL.hash_auxiliary_data(aux).to_hex() : undefined
  );
  const witness = signed.witness_set().vkeys()!.get(0);
  expect(
    witness
      .vkey()
      .public_key()
      .verify(
        blake2b(signed.body().to_bytes(), undefined, 32),
        witness.signature()
      )
  ).toBe(true);
  expect(
    signed
      .body()
      .voting_procedures()!
      .get(
        CSL.Voter.new_drep_credential(credential),
        CSL.GovernanceActionId.new(CSL.TransactionHash.from_hex(actionHash), 0)
      )!
      .vote_kind()
  ).toBe(1);
  // Independent CBOR decoder: assertions in tests use literal CIP-179 wire values.
  if (!aux) return undefined;
  const payload = new Decoder({ mapsAsObjects: false }).decode(
    aux.metadata()!.get(CSL.BigNum.from_str("17"))!.to_bytes()
  );
  expect(payload[0]).toBe(1);
  expect(payload[1]).toHaveLength(1);
  const response = payload[1][0] as Map<number, any>;
  expect(response.get(0)).toBe(5);
  expect(Buffer.from(response.get(1)[0]).toString("hex")).toBe(surveyHash);
  expect(response.get(1)[1]).toBe(0);
  expect(response.get(2)).toBe(0);
  expect(response.get(3)[0]).toBe(0);
  expect(Buffer.from(response.get(3)[1]).toString("hex")).toBe(
    key.to_public().hash().to_hex()
  );
  return response.get(4);
}
