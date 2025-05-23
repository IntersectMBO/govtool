import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";
import { useNavigate } from "react-router-dom";

import I18n from "@/i18n";
import { BUDGET_DISCUSSION_PATHS, PATHS, PDF_PATHS } from "@/consts";
import { useCardano, useModal } from "@/context";

import { Typography, Button } from "../../atoms";
import { HomeCard } from "../../molecules/HomeCard";

const isProposalDiscussionForumEnabled = JSON.parse(
  import.meta.env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED || false,
);

const CONNECT_WALLET_TO_CARDS = [
  ...(isProposalDiscussionForumEnabled
    ? [
        {
          title: I18n.t(
            "home.connectWalletTo.cards.discussBudgetProposals.title",
          ),
          description: I18n.t(
            "home.connectWalletTo.cards.discussBudgetProposals.description",
          ),
          path: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
        },
        {
          title: I18n.t(
            "home.connectWalletTo.cards.createBudgetProposal.title",
          ),
          description: I18n.t(
            "home.connectWalletTo.cards.createBudgetProposal.description",
          ),
          path: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
        },
        {
          title: I18n.t(
            "home.connectWalletTo.cards.discussGovernanceActions.title",
          ),
          description: I18n.t(
            "home.connectWalletTo.cards.discussGovernanceActions.description",
          ),
          path: PDF_PATHS.proposalDiscussion,
        },
        {
          title: I18n.t(
            "home.connectWalletTo.cards.proposeGovernanceAction.title",
          ),
          description: I18n.t(
            "home.connectWalletTo.cards.proposeGovernanceAction.description",
          ),
          path: PDF_PATHS.proposalDiscussionPropose,
        },
      ]
    : []),
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
  const { isEnabled, stakeKey } = useCardano();
  const navigate = useNavigate();

  const handleCardClick = ({ path }: { path?: string }) => {
    if (!path) return;
    openModal({
      type: "chooseWallet",
      state: {
        pathToNavigate: path,
      },
    });
  };

  const onClickConnectButton = () => {
    if (isEnabled && stakeKey) {
      navigate(PATHS.dashboard);
    }
    openModal({ type: "chooseWallet" });
  };

  return (
    <Box my={4} component="section" data-testid="connect-wallet-to-section">
      <Box display="flex" justifyContent="space-between" alignItems="center">
        <Typography variant="title">
          {t("home.connectWalletTo.section.title")}
        </Typography>
        <Button
          data-testid="connect-wallet-button"
          onClick={onClickConnectButton}
          size="extraLarge"
          variant="contained"
        >
          {t("wallet.connectWallet")}
        </Button>
      </Box>
      <Box
        display="grid"
        gridTemplateColumns={{
          xxs: "repeat(1, 1fr)",
          sm: "repeat(2, 1fr)",
          lg: "repeat(3, 1fr)",
        }}
        gap={3.75}
        mt={3.75}
      >
        {CONNECT_WALLET_TO_CARDS.map(({ title, description, path }) => (
          <HomeCard
            key={title}
            title={title}
            description={description}
            onCardClick={path ? () => handleCardClick({ path }) : undefined}
          />
        ))}
      </Box>
    </Box>
  );
};
