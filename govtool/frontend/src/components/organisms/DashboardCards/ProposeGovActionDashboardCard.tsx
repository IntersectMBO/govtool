import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { PendingTransaction } from "@context";
import { useTranslation, useWalletErrorModal } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { correctAdaFormat, openInNewTab } from "@utils";
import { useCallback } from "react";

type ProposeGovActionDashboardCardProps = {
  createGovActionTx: PendingTransaction["createGovAction"];
  deposit: number;
  votingPower: number;
};

export const ProposeGovActionDashboardCard = ({
  createGovActionTx,
  deposit,
  votingPower,
}: ProposeGovActionDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();
  const openWalletErrorModal = useWalletErrorModal();

  const onClickPropose = useCallback(() => {
    if (votingPower <= deposit) {
      openWalletErrorModal({
        error: t("errors.insufficientBalanceDescription", {
          ada: correctAdaFormat(deposit),
        }),
        title: t("errors.insufficientBalanceTitle"),
        dataTestId: "insufficient-balance-error-modal",
      });
      return;
    }

    navigate(PATHS.createGovernanceAction);
  }, [deposit, votingPower]);

  return (
    <DashboardActionCard
      buttons={[
        ...(createGovActionTx
          ? // transaction in progress
            [
              {
                children: t("dashboard.cards.proposeGovernanceAction.view"),
                dataTestId: "propose-governance-actions-button",
                onClick: () => navigate(PATHS.dashboardGovernanceActions),
                variant: "contained",
              } as const,
            ]
          : // default
            [
              {
                children: t("dashboard.cards.proposeGovernanceAction.propose"),
                dataTestId: "propose-governance-actions-button",
                onClick: onClickPropose,
                variant: "contained",
              } as const,
            ]),
        // common
        {
          children: t("learnMore"),
          dataTestId: "learn-more-button",
          onClick: () =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/what-is-a-governance-action",
            ),
        },
      ]}
      description={t("dashboard.cards.proposeGovernanceAction.description")}
      imageURL={IMAGES.proposeGovActionImage}
      isInProgressOnCard={false}
      transactionId={createGovActionTx?.transactionHash}
      state={createGovActionTx ? "inProgress" : "default"}
      title={t("dashboard.cards.proposeGovernanceAction.title")}
    />
  );
};
