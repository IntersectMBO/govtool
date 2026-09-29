import type {
  Account,
  DRepVotingPowerListResponse,
  EpochParams,
  MetadataValidationStatus,
} from "@/models";

export type ProposalDiscussionProps = {
  pdfApiUrl: string;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  walletAPI: any;
  /**
   * `connecting` keeps the pdf session without validating it; only
   * `disconnected` clears it. `ready` means address and stakeKey are set.
   */
  walletStatus?: "disconnected" | "connecting" | "ready";
  /** Unused: pdf-ui routes on react-router's location. */
  pathname?: string;
  locale?: string;
  validateMetadata: ({
    url,
    hash,
    standard,
  }: {
    url: string;
    hash: string;
    standard: "CIP108";
  }) => Promise<
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    | { status?: MetadataValidationStatus; metadata?: any; valid: boolean }
    | undefined
  >;
  fetchDRepVotingPowerList: (
    identifiers: string[],
  ) => Promise<DRepVotingPowerListResponse>;
  epochParams?: EpochParams;
  /** Accept an optional :port in URL fields (GovTool test mode only). */
  allowUrlPorts?: boolean;
  /** Unused: pdf-ui owns the username and mirrors it through setUsername. */
  username?: string;
  setUsername: (username: string) => void;
  getAdaHolderVotingPower: ({
    stakeKey,
  }: {
    stakeKey?: string;
  }) => Promise<number>;
  getAccount: ({ stakeKey }: { stakeKey?: string }) => Promise<Account>;
};

export default function ProposalDiscussion(
  props: ProposalDiscussionProps,
): JSX.Element;
