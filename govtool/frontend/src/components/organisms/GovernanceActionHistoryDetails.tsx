import { Box, CircularProgress } from "@mui/material";

import {
  useGetGovernanceActionMetadata,
  useGetGovernanceActionRecordQuery,
  useGetGovernanceActionEpochParams,
  useGetGovernanceActionProposalDiscussionQuery,
  useTranslation,
} from "@hooks";
import { GovernanceActionHistoryEmptyState } from "@molecules";
import { GovernanceActionRecord } from "@models";

import { outcomeContent } from "@/utils/outcomeDetails";

import { OutcomeDetails } from "./OutcomeDetails/OutcomeDetails";

const CenteredBox = ({ children }: { children: React.ReactNode }) => (
  <Box
    data-testid="single-action-page-circular-loader"
    sx={{
      alignItems: "center",
      display: "flex",
      flex: 1,
      justifyContent: "center",
      minHeight: "75vh",
      width: "100%",
    }}
  >
    {children}
  </Box>
);

type GovernanceActionHistoryDetailsContentProps = {
  governanceAction: GovernanceActionRecord;
};

const GovernanceActionHistoryDetailsContent = ({
  governanceAction,
}: GovernanceActionHistoryDetailsContentProps) => {
  const { metadata, metadataValid, isMetadataLoading } =
    useGetGovernanceActionMetadata(governanceAction);
  const { proposal, isProposalLoading } = useGetGovernanceActionProposalDiscussionQuery(
    governanceAction.tx_hash,
  );
  const { epochParams } = useGetGovernanceActionEpochParams(governanceAction);
  const content = outcomeContent(governanceAction, metadata, metadataValid);

  return (
    <OutcomeDetails
      abstract={content.abstract}
      action={governanceAction}
      authors={content.authors}
      epochParams={epochParams ?? undefined}
      isMetadataLoading={isMetadataLoading}
      isProposalLoading={isProposalLoading}
      metadataStatus={content.metadataStatus}
      motivation={content.motivation}
      proposal={proposal?.data ?? undefined}
      rationale={content.rationale}
      references={content.references}
      title={content.title}
    />
  );
};

type GovernanceActionHistoryDetailsProps = {
  /** CIP-105 `txHash#index` or CIP-129 `gov_action1…`. */
  id: string;
};

export const GovernanceActionHistoryDetails = ({ id }: GovernanceActionHistoryDetailsProps) => {
  const { t } = useTranslation();
  const { governanceAction, isGovernanceActionLoading, governanceActionError } =
    useGetGovernanceActionRecordQuery(id);

  if (isGovernanceActionLoading) {
    return (
      <CenteredBox>
        <CircularProgress />
      </CenteredBox>
    );
  }

  if (governanceActionError || !governanceAction) {
    return (
      <CenteredBox>
        <GovernanceActionHistoryEmptyState
          title={t("actionRecord.noResults.title")}
          description={t("actionRecord.noResults.description")}
        />
      </CenteredBox>
    );
  }

  return <GovernanceActionHistoryDetailsContent governanceAction={governanceAction} />;
};
