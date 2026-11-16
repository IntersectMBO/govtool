import { Box } from "@mui/material";

import { useCardano, useFeatureFlag } from "@context";
import { useTranslation } from "@hooks";
import { PageTabs } from "@molecules";
import { PDF_PATHS } from "@/consts/paths";
import { useGovernanceActionTabs } from "@/hooks/useGovernanceActionTabs";

/** Proposed, Live Voting and Outcomes; Proposed only while the forum is on. */
export const GovernanceActionsTabs = () => {
  const { isEnabled, stakeKey } = useCardano();
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { t } = useTranslation();
  const tabs = useGovernanceActionTabs(Boolean(isEnabled && stakeKey)).filter(
    (tab) =>
      isProposalDiscussionForumEnabled ||
      tab.to !== PDF_PATHS.proposalDiscussion,
  );

  return (
    <Box sx={{ mb: 3 }}>
      <PageTabs label={t("govActions.categoryTitle")} tabs={tabs} />
    </Box>
  );
};
