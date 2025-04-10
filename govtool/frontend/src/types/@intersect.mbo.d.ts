enum MetadataValidationStatus {
  URL_NOT_FOUND = "URL_NOT_FOUND",
  INVALID_JSONLD = "INVALID_JSONLD",
  INVALID_HASH = "INVALID_HASH",
  INCORRECT_FORMAT = "INCORRECT_FORMAT",
}
declare module "@intersect.mbo/pdf-ui/cjs" {
  import { EpochParams } from "@/models";

  type ProposalDiscussionProps = {
    pdfApiUrl: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    walletAPI: any;
    pathname: string;
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
    votingPower: number;
    username: string;
    setUsername: (username: string) => void;
  };

  type GovernanceActionsOutcomesProps = {
    apiUrl?: string;
    ipfsGateway?: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    walletAPI?: any;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    i18n?: any;
  };

  export default function ProposalDiscussion(
    props: ProposalDiscussionProps,
  ): JSX.Element;
}

declare module "@intersect.mbo/govtool-outcomes-pillar-ui/dist/esm" {
  export default function GovernanceActionsOutcomes(
    props: GovernanceActionsOutcomesProps,
  ): JSX.Element;
}
