import { act, renderHook, waitFor } from "@testing-library/react";
import * as CSL from "@emurgo/cardano-serialization-lib-asmjs";
import { blake2b } from "blakejs";
import {
  encodeMetadata,
  type MetadatumMap,
  type SurveyResponse,
} from "cip-179";

import { useDelegateTodRep } from "@/hooks/useDelegateToDrep";
import { CardanoProvider, useCardano } from "./wallet";

const mocks = vi.hoisted(() => ({
  storage: new Map<string, unknown>(),
  pending: vi.fn(() => false),
  update: vi.fn(),
  transactionStatus: vi.fn(async () => ({ transactionConfirmed: true })),
  walletErrorModal: vi.fn(),
  useCardano: (() => undefined) as () => unknown,
  appContext: {
    epochParams: undefined as unknown,
    ensureEpochParams: vi.fn(async (): Promise<unknown> => undefined),
  },
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
  useGetVoterInfo: () => ({ voter: undefined }),
  useWalletErrorModal: () => mocks.walletErrorModal,
}));
vi.mock("@services", () => ({
  getTransactionStatus: mocks.transactionStatus,
}));
vi.mock("@consts", () => ({
  PATHS: { home: "/" },
  COMPILED_GUARDRAIL_SCRIPT: "",
}));
vi.mock(".", () => ({
  useAppContext: () => ({ networkName: "Preview", ...mocks.appContext }),
  useModal: () => ({ openModal: vi.fn(), closeModal: vi.fn() }),
  useSnackbar: () => ({ addSuccessAlert: vi.fn() }),
  useCardano: () => mocks.useCardano(),
}));
vi.mock("react-router", () => ({ useNavigate: () => vi.fn() }));
vi.mock("./pendingTransaction", () => ({
  usePendingTransaction: () => ({
    isPendingTransaction: mocks.pending,
    updateTransaction: mocks.update,
    pendingTransaction: {},
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

mocks.useCardano = useCardano;

const connect = async ({ registered = true } = {}) => {
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
      getRegisteredPubStakeKeys: vi.fn(async () => (
        registered ? [key.to_public().to_hex()] : []
      )),
      getUnregisteredPubStakeKeys: async () => (
        registered ? [] : [key.to_public().to_hex()]
      ),
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
  // The delegation hook reads the same provider through the mocked context.
  const hook = renderHook(
    () => ({ ...useCardano(), delegation: useDelegateTodRep() }),
    { wrapper: CardanoProvider },
  );
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

const params = {
  min_fee_a: 44,
  min_fee_b: 155381,
  pool_deposit: 500000000,
  key_deposit: 2000000,
  coins_per_utxo_size: 4310,
  max_val_size: 5000,
  max_tx_size: 16384,
};

beforeEach(() => {
  vi.clearAllMocks();
  mocks.pending.mockReturnValue(false);
  mocks.transactionStatus.mockResolvedValue({ transactionConfirmed: true });
  mocks.storage.clear();
  mocks.storage.set("network_fixture", true);
  mocks.appContext.epochParams = params;
  mocks.appContext.ensureEpochParams.mockResolvedValue(params);
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

describe("stake key registration across delegations", () => {
  const connectDelegator = async (options?: { registered: boolean }) => {
    const wallet = await connect(options);
    const delegate = async (target: string) => {
      await act(async () => {
        await wallet.hook.result.current.delegation.delegate(target);
      });
    };
    return { ...wallet, delegate };
  };
  const registrations = (hex: string) => {
    const certs = CSL.Transaction.from_hex(hex).body().certs();
    let count = 0;
    for (let i = 0; i < (certs?.len() ?? 0); i++) {
      if (certs?.get(i).as_stake_registration()) count++;
    }
    return count;
  };
  const submitted = (api: Awaited<ReturnType<typeof connect>>["api"]) =>
    api.submitTx.mock.calls.map(([hex]) => registrations(hex));

  it("registers an unregistered key once, then delegates without registering again", async () => {
    const wallet = await connectDelegator({ registered: false });
    await wallet.delegate("abstain");
    // The wallet's CIP-95 list still lags after the first transaction.
    await wallet.delegate("no_confidence");
    await wallet.delegate(key.to_public().hash().to_hex());

    expect(mocks.walletErrorModal).not.toHaveBeenCalled();
    expect(submitted(wallet.api)).toEqual([1, 0, 0]);
    expect(wallet.hook.result.current.isStakeKeyRegistered()).toBe(true);
  });

  it("registers again when the registering transaction never confirmed", async () => {
    mocks.transactionStatus.mockResolvedValue({ transactionConfirmed: false });
    const wallet = await connectDelegator({ registered: false });
    await wallet.delegate("abstain");
    await waitFor(() =>
      expect(wallet.hook.result.current.isStakeKeyRegistered()).toBe(false),
    );
    await wallet.delegate("abstain");

    expect(submitted(wallet.api)).toEqual([1, 1]);
  });

  it("never registers a key the wallet reports as registered", async () => {
    const wallet = await connectDelegator({ registered: true });
    await wallet.delegate("abstain");
    await wallet.delegate("no_confidence");

    expect(mocks.walletErrorModal).not.toHaveBeenCalled();
    expect(submitted(wallet.api)).toEqual([0, 0]);
    expect(mocks.transactionStatus).not.toHaveBeenCalled();
  });
});

describe("wallet disconnect", () => {
  it("clears the DRep identity so the next wallet cannot inherit it", async () => {
    const wallet = await connect();
    expect(wallet.hook.result.current.dRepID).toBe(
      key.to_public().hash().to_hex(),
    );
    await act(async () => {
      await wallet.hook.result.current.disconnectWallet();
    });
    expect(wallet.hook.result.current.isEnabled).toBe(false);
    expect(wallet.hook.result.current.dRepID).toBe("");
    expect(wallet.hook.result.current.pubDRepKey).toBe("");
  });
});

describe("protocol parameters on a first visit", () => {
  const vote = async (wallet: Awaited<ReturnType<typeof connect>>) => {
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
      });
    });
  };

  it("builds once the params arrive from the app context after mount", async () => {
    // Empty localStorage and a bootstrap fetch that has not landed yet.
    mocks.appContext.epochParams = undefined;
    mocks.appContext.ensureEpochParams.mockReturnValue(new Promise(() => {}));
    const wallet = await connect();

    mocks.appContext.epochParams = params;
    wallet.hook.rerender();
    await vote(wallet);

    checkSigned(wallet.transactions().unsigned, wallet.transactions().signed);
    expect(mocks.appContext.ensureEpochParams).not.toHaveBeenCalled();
    expect(wallet.api.submitTx).toHaveBeenCalledTimes(1);
  });

  it("waits for the params fetch when a transaction starts before it lands", async () => {
    mocks.appContext.epochParams = undefined;
    mocks.appContext.ensureEpochParams.mockImplementation(
      () =>
        new Promise((resolve) => {
          setTimeout(() => resolve(params), 10);
        }),
    );
    const wallet = await connect();

    await vote(wallet);

    expect(mocks.appContext.ensureEpochParams).toHaveBeenCalled();
    expect(wallet.api.submitTx).toHaveBeenCalledTimes(1);
  });

  it("uses the app context, not a stale localStorage copy", async () => {
    mocks.storage.set("params", { ...params, key_deposit: 9000000 });
    const wallet = await connect({ registered: false });

    await act(async () => {
      await wallet.hook.result.current.delegation.delegate("abstain");
    });

    const [hex] = wallet.api.submitTx.mock.calls[0];
    const registration = CSL.Transaction.from_hex(hex)
      .body()
      .certs()
      ?.get(0)
      .as_stake_registration();
    expect(registration?.coin()?.to_str()).toBe("2000000");
  });

  it("fails without submitting when the params cannot be fetched", async () => {
    mocks.appContext.epochParams = undefined;
    mocks.appContext.ensureEpochParams.mockResolvedValue(undefined);
    const wallet = await connect();
    const votingBuilder = await wallet.hook.result.current.buildVote(
      "yes",
      actionHash,
      2,
    );

    await expect(
      wallet.hook.result.current.buildSignSubmitConwayCertTx({
        votingBuilder,
        type: "vote",
        resourceId: "fixture",
      }),
    ).rejects.toThrow("errors.appCannotCreateTransaction");
    expect(wallet.api.signTx).not.toHaveBeenCalled();
    expect(wallet.api.submitTx).not.toHaveBeenCalled();
  });
});
