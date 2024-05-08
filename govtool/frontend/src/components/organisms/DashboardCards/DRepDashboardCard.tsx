import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { PendingTransaction } from "@context";
import { useTranslation } from "@hooks";
import { VoterInfo } from "@models";
import {
  CopyableInfo,
  DashboardActionCard,
  DashboardActionCardProps,
} from "@molecules";
import { correctAdaFormat, openInNewTab } from "@utils";

type DRepDashboardCardProps = {
  dRepIDBech32: string;
  pendingTransaction: PendingTransaction;
  voter: VoterInfo;
};

export const DRepDashboardCard = ({
  dRepIDBech32,
  pendingTransaction,
  voter,
}: DRepDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const inProgress = !!(
    pendingTransaction.registerAsDrep ||
    pendingTransaction.retireAsDrep ||
    pendingTransaction.updateMetaData
  );

  const learnMoreButton = {
    children: t("learnMore"),
    dataTestId: "register-learn-more-button",
    onClick: () =>
      openInNewTab(
        "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep",
      ),
  };

  const navigateToDrepDirectory = () =>
    navigate(
      PATHS.dashboardDRepDirectoryDRep.replace(":dRepId", dRepIDBech32),
      { state: { enteredFromWithinApp: true } },
    );

  const cardProps: Partial<DashboardActionCardProps> = (() => {
    // transaction in progress
    if (inProgress) {
      return {
        buttons: [learnMoreButton],
        state: "inProgress",
        ...(pendingTransaction.registerAsDrep && {
          description: t("dashboard.cards.drep.registrationInProgress"),
          transactionId: pendingTransaction?.registerAsDrep.transactionHash,
          title: t("dashboard.cards.drep.dRepRegistration"),
        }),
        ...(pendingTransaction.retireAsDrep && {
          description: (
            <Trans
              i18nKey="dashboard.cards.drep.retirementInProgress"
              values={{ deposit: correctAdaFormat(voter?.deposit) }}
            />
          ),
          transactionId: pendingTransaction?.retireAsDrep.transactionHash,
          title: t("dashboard.cards.drep.dRepRetirement"),
        }),
        ...(pendingTransaction.updateMetaData && {
          description: t("dashboard.cards.drep.metadataUpdateInProgress"),
          transactionId: pendingTransaction?.updateMetaData.transactionHash,
          title: t("dashboard.cards.drep.dRepUpdate"),
        }),
      };
    }

    // currently registered
    if (voter?.isRegisteredAsDRep) {
      return {
        buttons: [
          {
            children: t("dashboard.cards.drep.viewDetails"),
            dataTestId: "view-drep-details-button",
            onClick: navigateToDrepDirectory,
            variant: "outlined",
            sx: { backgroundColor: "arcticWhite" },
          },
          {
            children: t("dashboard.cards.drep.retire"),
            dataTestId: "retire-button",
            onClick: () => navigate(PATHS.retireAsDrep),
            variant: "text",
          },
        ],
        description: t("dashboard.cards.drep.registeredDescription"),
        state: "active",
        transactionId: voter?.dRepRegisterTxHash,
        title: t("dashboard.cards.drep.registeredTitle"),
      };
    }

    // common buttons for was registered or not registered
    const wasRegisteredOrNotRegisteredButtons: DashboardActionCardProps["buttons"] =
      [
        {
          children: t(
            voter.wasRegisteredAsDRep
              ? "dashboard.cards.drep.reRegister"
              : "dashboard.cards.drep.register",
          ),
          dataTestId: "register-button",
          onClick: () => navigate(PATHS.registerAsdRep),
          variant: "contained",
        },
        learnMoreButton,
      ];

    // was registered
    if (voter?.wasRegisteredAsDRep) {
      return {
        buttons: wasRegisteredOrNotRegisteredButtons,
        description: (
          <Trans i18nKey="dashboard.cards.drep.notRegisteredWasRegisteredDescription" />
        ),
        transactionId: voter?.dRepRetireTxHash,
        title: t("dashboard.cards.drep.notRegisteredWasRegisteredTitle"),
      };
    }

    // not registered
    return {
      buttons: wasRegisteredOrNotRegisteredButtons,
      description: t("dashboard.cards.drep.notRegisteredDescription"),
      title: t("dashboard.cards.drep.notRegisteredTitle"),
    };
  })();

  return (
    <DashboardActionCard
      imageURL={IMAGES.govActionRegisterImage}
      {...cardProps}
    >
      {voter?.isRegisteredAsDRep && !pendingTransaction?.retireAsDrep && (
        <CopyableInfo
          dataTestId="my-drep-id"
          label={t("dashboard.cards.drep.yourDRepId")}
          sx={{ mt: 1 }}
          value={dRepIDBech32}
        />
      )}
    </DashboardActionCard>
  );
};
