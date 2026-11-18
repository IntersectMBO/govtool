import { useLocation, useNavigate } from "react-router";
import { Box, Button, Typography } from "@mui/material";

import {
  PDF_PATHS,
  PATHS,
  gray,
  GOV_ACTION_HISTORY_PATHS,
  BUDGET_DISCUSSION_PATHS,
} from "@consts";
import { useCardano, useModal } from "@context";
import { useTranslation } from "@hooks";
import { Card } from "./Card";

export const WalletInfoCard = () => {
  const { address, disconnectWallet } = useCardano();
  const { openModal } = useModal();
  const navigate = useNavigate();
  const { pathname, hash } = useLocation();
  const { t } = useTranslation();

  const onClickDisconnect = async () => {
    await disconnectWallet();

    const isBudgetDiscussion = window.location.pathname.includes(
      BUDGET_DISCUSSION_PATHS.budgetDiscussion.replace("/", ""),
    );

    const isProposalDiscussionForum = window.location.pathname.includes(
      PDF_PATHS.proposalDiscussion.replace("/", ""),
    );

    const isGovernanceGovernanceActionHistoryPillar = window.location.pathname.includes(
      GOV_ACTION_HISTORY_PATHS.governanceActionHistory.replace("/", ""),
    );
    if (
      !isBudgetDiscussion &&
      !isProposalDiscussionForum &&
      !isGovernanceGovernanceActionHistoryPillar
    ) {
      navigate(
        pathname.includes("/connected")
          ? `${pathname.replace("/connected", "")}${hash ?? ""}`
          : PATHS.home,
      );
    }

    window.location.reload();
  };

  return (
    address && (
      <Card border elevation={0} sx={{ p: 1.5 }}>
        <Typography color={gray.c300} fontSize={12} fontWeight={500}>
          {t("wallet.connectedWallet")}
        </Typography>
        <Box sx={{ alignItems: "center", display: "flex" }}>
          <Typography
            sx={{
              flex: 1,
              fontSize: 14,
              fontWeight: 400,
              overflow: "hidden",
              textOverflow: "ellipsis",
              width: 10,
            }}
          >
            {address}
          </Typography>
          <Button
            data-testid="disconnect-button"
            variant="text"
            onClick={() =>
              openModal({
                type: "disconnectWallet",
                state: { onConfirm: onClickDisconnect },
              })}
          >
            {t("wallet.disconnect")}
          </Button>
        </Box>
        <Button
          data-testid="view-profile-button"
          fullWidth
          onClick={() => navigate(PATHS.profile)}
          sx={{ mt: 1 }}
          variant="outlined"
        >
          {t("walletConnect.viewProfile")}
        </Button>
      </Card>
    )
  );
};
