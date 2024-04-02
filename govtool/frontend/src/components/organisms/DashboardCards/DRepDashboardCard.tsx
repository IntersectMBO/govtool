import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import {
  CopyableInfo,
  DashboardActionCard,
  DashboardActionCardProps,
} from "@molecules";
import { openInNewTab } from "@utils";
import { PendingTransaction } from "@/context/pendingTransaction";
import { VoterInfo } from "@/models";

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

  const cardProps: Partial<DashboardActionCardProps> = (() => {
    // transaction in progress
    if (inProgress) {
      return {
        buttons: [
          {
            children: t("seeTransaction"),
            onClick: () =>
              openInNewTab("https://adanordic.com/latest_transactions"),
          },
        ],
        state: "inProgress",
        ...(pendingTransaction.registerAsDrep && {
          description: t("dashboard.registration.registrationInProgress"),
          title: t("dashboard.registration.dRepRegistration"),
        }),
        ...(pendingTransaction.retireAsDrep && {
          description: t("dashboard.registration.retirementInProgress"),
          title: t("dashboard.registration.dRepRetirement"),
        }),
        ...(pendingTransaction.updateMetaData && {
          description: t("dashboard.registration.metadataUpdateInProgress"),
          title: t("dashboard.registration.dRepUpdate"),
        }),
      };
    }

    // currently registered
    if (voter?.isRegisteredAsDRep) {
      return {
        buttons: [
          {
            children: t("dashboard.registration.retire"),
            dataTestId: "retire-button",
            onClick: () => navigate(PATHS.retireAsDrep),
          },
          {
            children: t("dashboard.registration.changeMetadata"),
            dataTestId: "change-metadata-button",
            onClick: () => navigate(PATHS.editDrepMetadata),
            variant: "text",
          },
        ],
        description: t("dashboard.registration.holdersCanDelegate"),
        state: "active",
        title: t("dashboard.registration.youAreRegistered"),
      };
    }

    // common buttons for was registered or not registered
    const wasRegisteredOrNotRegisteredButtons: DashboardActionCardProps["buttons"] =
      [
        {
          children: t(
            voter.wasRegisteredAsDRep
              ? "dashboard.registration.reRegister"
              : "dashboard.registration.register",
          ),
          dataTestId: "register-button",
          onClick: () => navigate(PATHS.registerAsdRep),
          variant: "contained",
        },
        {
          children: t("learnMore"),
          dataTestId: "register-learn-more-button",
          onClick: () =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep",
            ),
        },
      ];

    // was registered
    if (voter?.wasRegisteredAsDRep) {
      return {
        buttons: wasRegisteredOrNotRegisteredButtons,
        description: t("dashboard.registration.holdersCanDelegate"),
        title: t("dashboard.registration.registerAgain"),
      };
    }

    // not registered
    return {
      buttons: wasRegisteredOrNotRegisteredButtons,
      description: t("dashboard.registration.ifYouWant"),
      title: t("dashboard.registration.registerAsDRep"),
    };
  })();

  return (
    <DashboardActionCard
      imageURL={IMAGES.govActionRegisterImage}
      {...cardProps}
    >
      {(voter?.isRegisteredAsDRep || voter?.wasRegisteredAsDRep) && (
        <CopyableInfo
          dataTestId="my-drep-id"
          label={t("myDRepId")}
          value={dRepIDBech32}
        />
      )}
    </DashboardActionCard>
  );
};
