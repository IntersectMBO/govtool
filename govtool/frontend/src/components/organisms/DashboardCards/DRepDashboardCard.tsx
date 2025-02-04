import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { PendingTransaction } from "@context";
import { useGetDRepDetailsQuery, useTranslation } from "@hooks";
import { VoterInfo } from "@models";
import {
  CopyableInfo,
  DashboardActionCard,
  DashboardActionCardProps,
} from "@molecules";
import { correctAdaFormat, openInNewTab } from "@utils";
import { LINKS } from "@/consts/links";

type DRepDashboardCardProps = {
  dRepID: string;
  pendingTransaction: PendingTransaction;
  voter: VoterInfo;
};

export const DRepDashboardCard = ({
  dRepID,
  pendingTransaction,
  voter,
}: DRepDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const { dRep } = useGetDRepDetailsQuery(dRepID);

  const inProgress = !!(
    pendingTransaction.registerAsDrep ||
    pendingTransaction.retireAsDrep ||
    pendingTransaction.updateMetaData
  );

  const learnMoreButton = {
    children: t("learnMore"),
    dataTestId: "d-rep-learn-more-button",
    onClick: () => openInNewTab(LINKS.REGISTER_AS_A_DREP),
  };

  const navigateToDrepDirectory = () => {
    if (dRep) {
      navigate(PATHS.dashboardDRepDirectoryDRep.replace(":dRepId", dRep.view), {
        state: { enteredFromWithinApp: true },
      });
    }
  };

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
          description: voter?.givenName ? (
            <Trans
              i18nKey="dashboard.cards.drep.retirementInProgressWithGivenName"
              values={{
                deposit: correctAdaFormat(voter?.deposit),
                givenName: voter?.givenName,
              }}
            />
          ) : (
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
        voter.wasRegisteredAsDRep
          ? {
              children: t("learnMore"),
              dataTestId: "register-learn-more-button",
              onClick: () => openInNewTab(LINKS.RETIRE_AS_A_DREP),
            }
          : learnMoreButton,
      ];

    // was registered
    if (voter?.wasRegisteredAsDRep) {
      return {
        buttons: wasRegisteredOrNotRegisteredButtons,
        description: voter?.givenName ? (
          <Trans
            i18nKey="dashboard.cards.drep.notRegisteredWasRegisteredDescriptionWithGivenName"
            values={{ givenName: voter?.givenName ?? "DRep" }}
          />
        ) : (
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
      type="d-rep"
      {...cardProps}
    >
      {voter?.isRegisteredAsDRep &&
        !pendingTransaction?.retireAsDrep &&
        dRep && (
          <CopyableInfo
            dataTestId="dRep-id-display-card-dashboard"
            label={t("dashboard.cards.drep.yourDRepId")}
            sx={{ mt: 1 }}
            value={dRep.view}
          />
        )}
    </DashboardActionCard>
  );
};
