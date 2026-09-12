import { act, renderHook } from "@testing-library/react";
import * as CSL from "@emurgo/cardano-serialization-lib-asmjs";
import { blake2b } from "blakejs";
import {
  encodeMetadata,
  type MetadatumMap,
  type SurveyResponse,
} from "cip-179";

import { CardanoProvider, useCardano } from "./wallet";

const mocks = vi.hoisted(() => ({
  storage: new Map<string, unknown>(),
  pending: vi.fn(() => false),
  update: vi.fn(),
}));
vi.mock("@/config/env", () => ({ env: { VITE_NETWORK_FLAG: "0" } }));
vi.mock("@utils", () => ({
  PROTOCOL_PARAMS_KEY: "params",
  NETWORK_INFO_KEY: "network",
  WALLET_LS_KEY: "wallet",
  checkIsMaintenanceOn: vi.fn(),
  getItemFromLocalStorage: (key: string) => mocks.storage.get(key),
  setItemToLocalStorage: (key: string, value: unknown) =>
    mocks.storage.set(key, value),
  removeItemFromLocalStorage: (key: string) => mocks.storage.delete(key),
  getPubDRepID: async () => ({
    dRepID: key.to_public().hash().to_hex(),
    dRepKey: key.to_public().to_hex(),
  }),
}));
vi.mock("@hooks", () => ({
  useTranslation: () => ({ t: (key: string) => key }),
}));
vi.mock("@consts", () => ({
  PATHS: { home: "/" },
  COMPILED_GUARDRAIL_SCRIPT: "",
}));
vi.mock(".", () => ({
  useAppContext: () => ({ networkName: "Preview" }),
  useModal: () => ({ openModal: vi.fn(), closeModal: vi.fn() }),
  useSnackbar: () => ({ addSuccessAlert: vi.fn() }),
}));
vi.mock("react-router", () => ({ useNavigate: () => vi.fn() }));
vi.mock("./pendingTransaction", () => ({
  usePendingTransaction: () => ({
    isPendingTransaction: mocks.pending,
    updateTransaction: mocks.update,
  }),
}));
vi.mock("@sentry/react", () => ({
  addBreadcrumb: vi.fn(),
  setTag: vi.fn(),
  captureException: vi.fn(),
}));

// Public deterministic test key and fabricated UTxO; never use on a live network.
const key = CSL.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
const credential = CSL.Credential.from_keyhash(key.to_public().hash());
const address = CSL.BaseAddress.new(0, credential, credential).to_address();
const utxo = CSL.TransactionUnspentOutput.new(
  CSL.TransactionInput.new(CSL.TransactionHash.from_hex("33".repeat(32)), 0),
  CSL.TransactionOutput.new(
    address,
    CSL.Value.new(CSL.BigNum.from_str("5000000000")),
  ),
);
const actionHash = "44".repeat(32);
const response: SurveyResponse = {
  specVersion: 5,
  surveyRef: { txId: new Uint8Array(32), index: 1 },
  role: 0,
  credential: { type: "key", keyHash: key.to_public().hash().to_bytes() },
  answers: {
    type: "public",
    answers: [{ type: "singleChoice", questionIndex: 0, optionIndex: 0 }],
  },
};
const metadata = (value: SurveyResponse): MetadatumMap => {
  const encoded = encodeMetadata({ type: "responses", responses: [value] });
  if (!(encoded instanceof Map)) throw new Error("Expected metadata map");
  return encoded;
};

const connect = async () => {
  let unsigned: CSL.Transaction | undefined;
  let signed: CSL.Transaction | undefined;
  const api = {
    getChangeAddress: async () => address.to_hex(),
    getUsedAddresses: async () => [address.to_hex()],
    getUnusedAddresses: async () => [],
    getExtensions: async () => [{ cip: 95 }],
    getNetworkId: async () => 0,
    getUtxos: async () => [utxo.to_hex()],
    cip95: {
      getRegisteredPubStakeKeys: async () => [key.to_public().to_hex()],
      getUnregisteredPubStakeKeys: async () => [],
    },
    signTx: vi.fn(async (hex: string, partial: boolean) => {
      expect(partial).toBe(true);
      unsigned = CSL.Transaction.from_hex(hex);
      const hash = CSL.TransactionHash.from_bytes(
        blake2b(unsigned.body().to_bytes(), undefined, 32),
      );
      const witnesses = CSL.TransactionWitnessSet.new();
      const vkeys = CSL.Vkeywitnesses.new();
      vkeys.add(CSL.make_vkey_witness(hash, key));
      witnesses.set_vkeys(vkeys);
      return witnesses.to_hex();
    }),
    submitTx: vi.fn(async (hex: string) => {
      signed = CSL.Transaction.from_hex(hex);
      return "55".repeat(32);
    }),
  };
  vi.stubGlobal("cardano", {
    fixture: {
      supportedExtensions: [{ cip: 95 }],
      enable: async () => api,
      isEnabled: async () => true,
    },
  });
  const hook = renderHook(() => useCardano(), { wrapper: CardanoProvider });
  await act(async () => {
    await hook.result.current.enable("fixture");
  });
  expect(hook.result.current.isEnabled).toBe(true);
  return {
    hook,
    api,
    transactions: () => {
      if (!unsigned || !signed)
        throw new Error("Expected signTx and submitTx calls");
      return { unsigned, signed };
    },
  };
};

beforeEach(() => {
  vi.clearAllMocks();
  mocks.pending.mockReturnValue(false);
  mocks.storage.clear();
  mocks.storage.set("network_fixture", true);
  mocks.storage.set("params", {
    min_fee_a: 44,
    min_fee_b: 155381,
    pool_deposit: 500000000,
    key_deposit: 2000000,
    coins_per_utxo_size: 4310,
    max_val_size: 5000,
    max_tx_size: 16384,
  });
  vi.spyOn(console, "log").mockImplementation(() => {});
});
afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

const checkSigned = (unsigned: CSL.Transaction, signed: CSL.Transaction) => {
  expect(signed.body().to_hex()).toBe(unsigned.body().to_hex());
  expect(signed.auxiliary_data()?.to_hex()).toBe(
    unsigned.auxiliary_data()?.to_hex(),
  );
  const auxiliary = signed.auxiliary_data();
  expect(signed.body().auxiliary_data_hash()?.to_hex()).toBe(
    auxiliary ? CSL.hash_auxiliary_data(auxiliary).to_hex() : undefined,
  );
  const witness = signed.witness_set().vkeys()!.get(0);
  expect(
    witness
      .vkey()
      .public_key()
      .verify(
        blake2b(signed.body().to_bytes(), undefined, 32),
        witness.signature(),
      ),
  ).toBe(true);
};

describe("wallet transaction submission", () => {
  it.each([
    ["ordinary vote", undefined],
    ["empty metadata", new Map()],
    ["public survey", metadata(response)],
    [
      "sealed survey",
      metadata({
        ...response,
        answers: { type: "sealed", ciphertext: new Uint8Array(97).fill(42) },
      }),
    ],
    ["unrelated metadata", new Map([[674n, "message"]])],
  ] as const)(
    "preserves body, metadata and witnesses for %s",
    async (_name, transactionMetadata) => {
      const wallet = await connect();
      const votingBuilder = await wallet.hook.result.current.buildVote(
        "yes",
        actionHash,
        2,
      );
      await act(async () => {
        await wallet.hook.result.current.buildSignSubmitConwayCertTx({
          votingBuilder,
          type: "vote",
          resourceId: "fixture",
          transactionMetadata,
        });
      });
      const { unsigned, signed } = wallet.transactions();
      expect(!!unsigned.auxiliary_data()).toBe(!!transactionMetadata?.size);
      checkSigned(unsigned, signed);
      const voter = CSL.Voter.new_drep_credential(credential);
      const action = CSL.GovernanceActionId.new(
        CSL.TransactionHash.from_hex(actionHash),
        2,
      );
      expect(
        signed.body().voting_procedures()?.get(voter, action)?.vote_kind(),
      ).toBe(1);
      expect(wallet.api.submitTx).toHaveBeenCalledTimes(1);
      expect(mocks.update).toHaveBeenCalledTimes(1);
    },
  );

  it("preserves a certificate transaction without auxiliary data", async () => {
    const wallet = await connect();
    const certBuilder = CSL.Certificate.new_stake_registration(
      CSL.StakeRegistration.new(credential),
    );
    await act(async () => {
      await wallet.hook.result.current.buildSignSubmitConwayCertTx({
        certBuilder,
        type: "registerAsDrep",
      });
    });
    const { unsigned, signed } = wallet.transactions();
    checkSigned(unsigned, signed);
    expect(signed.body().certs()?.get(0).to_hex()).toBe(certBuilder.to_hex());
    expect(signed.auxiliary_data()).toBeUndefined();
  });

  it("does not submit or mark a transaction pending when the wallet rejects signing", async () => {
    const wallet = await connect();
    wallet.api.signTx.mockRejectedValueOnce(new Error("User declined"));
    const votingBuilder = await wallet.hook.result.current.buildVote(
      "no",
      actionHash,
      2,
    );
    await expect(
      wallet.hook.result.current.buildSignSubmitConwayCertTx({
        votingBuilder,
        type: "vote",
        resourceId: "fixture",
        transactionMetadata: metadata(response),
      }),
    ).rejects.toThrow("User declined");
    expect(wallet.api.submitTx).not.toHaveBeenCalled();
    expect(mocks.update).not.toHaveBeenCalled();
  });

  it("preserves a governance-action proposal without survey metadata", async () => {
    const wallet = await connect();
    const govActionBuilder = CSL.VotingProposalBuilder.new();
    const proposal = CSL.VotingProposal.new(
      CSL.GovernanceAction.new_info_action(CSL.InfoAction.new()),
      CSL.Anchor.new(
        CSL.URL.new("https://example.com/action.jsonld"),
        CSL.AnchorDataHash.from_hex("66".repeat(32)),
      ),
      CSL.RewardAddress.new(0, credential),
      CSL.BigNum.from_str("1000000000"),
    );
    govActionBuilder.add(proposal);
    await act(async () => {
      await wallet.hook.result.current.buildSignSubmitConwayCertTx({
        govActionBuilder,
        type: "createGovAction",
      });
    });
    const { unsigned, signed } = wallet.transactions();
    checkSigned(unsigned, signed);
    expect(signed.body().voting_proposals()?.get(0).to_hex()).toBe(
      proposal.to_hex(),
    );
    expect(signed.auxiliary_data()).toBeUndefined();
  });
});
