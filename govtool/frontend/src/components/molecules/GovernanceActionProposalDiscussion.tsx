import { Box, Skeleton } from "@mui/material";
import { Link } from "react-router";

import { Button, Typography } from "@atoms";
import { PDF_PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { GovernanceActionProposalDiscussion as GovernanceActionProposalDiscussionData } from "@models";
import { formatGovernanceActionTimestamp } from "@utils";

type GovernanceActionProposalDiscussionProps = {
  proposal?: GovernanceActionProposalDiscussionData | null;
  isLoading: boolean;
};

/** The discussion forum proposal the action came from, if there is one. */
export const GovernanceActionProposalDiscussion = ({
  proposal,
  isLoading,
}: GovernanceActionProposalDiscussionProps) => {
  const { t } = useTranslation();

  const renderContent = () => {
    if (isLoading) {
      return (
        <Box data-testid="proposal-card-loader" display="flex" gap={2}>
          <Skeleton variant="rounded" width={216} height={40} />
          <Skeleton variant="rounded" width={116} height={40} />
        </Box>
      );
    }

    if (!proposal) {
      return (
        <Typography
          sx={{ fontSize: 16, fontWeight: 400, color: "neutralGray" }}
        >
          {t("actionRecord.proposalDiscussion.notFound")}
        </Typography>
      );
    }

    const typeName =
      proposal.attributes?.content?.attributes?.gov_action_type?.attributes
        ?.gov_action_type_name;
    const createdAt = proposal.attributes?.createdAt;

    return (
      <Box
        data-testid={`proposal-${typeName?.toLowerCase() ?? ""}-card`}
        display="flex"
        flexDirection="row"
        alignItems="center"
        gap={2}
      >
        <Box
          sx={{
            backgroundColor: "#B8CDFF",
            borderRadius: "14px",
            px: 1,
            py: 1,
          }}
        >
          <Typography variant="body2" data-testid="proposed-date-wrapper">
            {t("actionRecord.proposalDiscussion.proposedOn")}
            <Box component="span" data-testid="proposed-date" ml={0.5}>
              {createdAt ? formatGovernanceActionTimestamp(createdAt, "short") : "-"}
            </Box>
          </Typography>
        </Box>
        <Button
          component={Link}
          to={`${PDF_PATHS.proposalDiscussion}/${proposal.id}`}
          data-testid={`proposal-${proposal.id}-view-details`}
        >
          {t("actionRecord.seeDiscussion")}
        </Button>
      </Box>
    );
  };

  return (
    <Box display="flex" flexDirection="column" gap={0.5}>
      <Typography
        data-testid="related-proposal-label"
        sx={{ color: "neutralGray", fontWeight: 600, fontSize: 14 }}
      >
        {t("actionRecord.proposalDiscussion.title")}
      </Typography>
      {renderContent()}
    </Box>
  );
};
