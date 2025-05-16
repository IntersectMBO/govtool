import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";

import I18n from "@/i18n";
import { BUDGET_DISCUSSION_PATHS, PATHS, PDF_PATHS } from "@/consts";

import { Typography } from "../../atoms";
import { HomeCard } from "../../molecules/HomeCard";
import { useModal } from "@/context";

const CONNECT_WALLET_TO_CARDS = [
  {
    title: I18n.t("home.connectWalletTo.cards.discussBudgetProposals.title"),
    description: I18n.t(
      "home.connectWalletTo.cards.discussBudgetProposals.description",
    ),
    path: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
  },
  {
    title: I18n.t("home.connectWalletTo.cards.createBudgetProposal.title"),
    description: I18n.t(
      "home.connectWalletTo.cards.createBudgetProposal.description",
    ),
    path: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
  },
  {
    title: I18n.t("home.connectWalletTo.cards.discussGovernanceActions.title"),
    description: I18n.t(
      "home.connectWalletTo.cards.discussGovernanceActions.description",
    ),
    path: PDF_PATHS.proposalDiscussion,
  },
  {
    title: I18n.t("home.connectWalletTo.cards.proposeGovernanceAction.title"),
    description: I18n.t(
      "home.connectWalletTo.cards.proposeGovernanceAction.description",
    ),
    path: PDF_PATHS.proposalDiscussionPropose,
  },
  {
    title: I18n.t("home.connectWalletTo.cards.registerToVote.title"),
    description: I18n.t(
      "home.connectWalletTo.cards.registerToVote.description",
    ),
    path: PATHS.registerAsDirectVoter,
  },
  {
    title: I18n.t("home.connectWalletTo.cards.delegateVote.title"),
    description: I18n.t("home.connectWalletTo.cards.delegateVote.description"),
    path: PATHS.dRepDirectory,
  },
  {
    title: I18n.t("home.connectWalletTo.cards.becomeDRep.title"),
    description: I18n.t("home.connectWalletTo.cards.becomeDRep.description"),
    path: PATHS.registerAsdRep,
  },
  {
    title: I18n.t("home.connectWalletTo.cards.voteOnGovernanceActions.title"),
    description: I18n.t(
      "home.connectWalletTo.cards.voteOnGovernanceActions.description",
    ),
    path: PATHS.dashboardGovernanceActions,
  },
];

export const ConnectWalletTo = () => {
  const { t } = useTranslation();
  const { openModal } = useModal();

  const handleCardClick = ({ path }: { path?: string }) => {
    if (!path) return;
    openModal({
      type: "chooseWallet",
      state: {
        pathToNavigate: path,
      },
    });
  };

  return (
    <Box my={4} component="section" data-testid="connect-wallet-to-section">
      <Typography variant="title2">
        {t("home.connectWalletTo.section.title")}
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
        {CONNECT_WALLET_TO_CARDS.map(({ title, description, path }) => (
          <HomeCard
            key={title}
            title={title}
            description={description}
            onCardClick={() => handleCardClick({ path })}
          />
        ))}
      </Box>
    </Box>
  );
};
