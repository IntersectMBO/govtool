import { Box } from "@mui/material";
import { useNavigate } from "react-router-dom";
import { useTranslation } from "react-i18next";

import I18n from "@/i18n";
import {
  BUDGET_DISCUSSION_PATHS,
  OUTCOMES_PATHS,
  PATHS,
  PDF_PATHS,
} from "@/consts";
import { openInNewTab } from "@/utils";

import { Typography } from "../../atoms";
import { HomeCard } from "../../molecules/HomeCard";

const isProposalDiscussionForumEnabled = JSON.parse(
  import.meta.env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED || false,
);

const isOutcomesEnabled = JSON.parse(
  import.meta.env.VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED || false,
);

const OPEN_TO_ANY_CARDS = [
  {
    title: I18n.t("home.openToAny.cards.drepDirectory.title"),
    description: I18n.t("home.openToAny.cards.drepDirectory.description"),
    path: PATHS.dRepDirectory,
  },
  ...(isProposalDiscussionForumEnabled
    ? [
        {
          title: I18n.t("home.openToAny.cards.budgetProposal.title"),
          description: I18n.t(
            "home.openToAny.cards.budgetProposal.description",
          ),
          path: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
        },
        {
          title: I18n.t("home.openToAny.cards.proposalDiscussion.title"),
          description: I18n.t(
            "home.openToAny.cards.proposalDiscussion.description",
          ),
          path: PDF_PATHS.proposalDiscussion,
        },
      ]
    : []),
  {
    title: I18n.t("home.openToAny.cards.governanceActions.title"),
    description: I18n.t("home.openToAny.cards.governanceActions.description"),
    path: PATHS.governanceActions,
  },
  ...(isOutcomesEnabled
    ? [
        {
          title: I18n.t("home.openToAny.cards.governanceOutcomes.title"),
          description: I18n.t(
            "home.openToAny.cards.governanceOutcomes.description",
          ),
          path: OUTCOMES_PATHS.governanceActionsOutcomes,
        },
      ]
    : []),
  {
    title: I18n.t("home.openToAny.cards.guides.title"),
    description: I18n.t("home.openToAny.cards.guides.description"),
    url: "https://docs.gov.tools/cardano-govtool/using-govtool",
  },
];

export const OpenToAny = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const handleCardClick = ({ path, url }: { path?: string; url?: string }) => {
    if (path) {
      navigate(path);
    } else if (url) {
      openInNewTab(url);
    }
  };

  return (
    <Box
      my={4}
      component="section"
      data-testid="open-to-any-and-all-users-section"
    >
      <Typography variant="title2">
        {t("home.openToAny.section.title")}
      </Typography>
      <Box
        display="grid"
        gridTemplateColumns={{
          xxs: "repeat(1, 1fr)",
          sm: "repeat(2, 1fr)",
          lg: "repeat(3, 1fr)",
        }}
        gap={4}
        mt={4}
      >
        {OPEN_TO_ANY_CARDS.map(({ title, description, path, url }) => (
          <HomeCard
            key={title}
            title={title}
            description={description}
            onCardClick={
              path || url ? () => handleCardClick({ path, url }) : undefined
            }
          />
        ))}
      </Box>
    </Box>
  );
};
