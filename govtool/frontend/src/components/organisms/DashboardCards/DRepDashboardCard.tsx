import { useCallback, useMemo } from "react";
import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { openInNewTab } from "@utils";
import { PendingTransaction } from "@/context/pendingTransaction";
import { VoterInfo } from "@/models";

type DRepDashboardCardProps = {
  dRepIDBech32: string;
  isPendingTransaction: () => boolean;
  pendingTransaction: PendingTransaction;
  voter: VoterInfo;
};

export const DRepDashboardCard = ({
  dRepIDBech32,
  isPendingTransaction,
  pendingTransaction,
  voter,
}: DRepDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const navigateTo = useCallback(
    (path: string) => {
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;
      navigate(path);
    },
    [isPendingTransaction, navigate]
  );

  const registrationCardDescription = useMemo(() => {
    if (pendingTransaction.registerAsDrep) return t("dashboard.registration.registrationInProgress");

    if (pendingTransaction.retireAsDrep) return t("dashboard.registration.retirementInProgress");

    if (pendingTransaction.updateMetaData) return t("dashboard.registration.metadataUpdateInProgress");

    if (voter?.isRegisteredAsDRep || voter?.wasRegisteredAsDRep) return t("dashboard.registration.holdersCanDelegate");

    return t("dashboard.registration.ifYouWant");
  }, [
    pendingTransaction,
    voter?.isRegisteredAsDRep,
    voter?.wasRegisteredAsDRep,
  ]);

  const registrationCardTitle = useMemo(() => {
    if (pendingTransaction.retireAsDrep) return t("dashboard.registration.dRepRetirement");

    if (pendingTransaction.registerAsDrep) return t("dashboard.registration.dRepRegistration");

    if (pendingTransaction.updateMetaData) return t("dashboard.registration.dRepUpdate");

    if (voter?.isRegisteredAsDRep) return t("dashboard.registration.youAreRegistered");

    if (voter?.wasRegisteredAsDRep) return t("dashboard.registration.registerAgain");

    return t("dashboard.registration.registerAsDRep");
  }, [
    pendingTransaction,
    voter?.isRegisteredAsDRep,
    voter?.wasRegisteredAsDRep,
  ]);

  return (
    <DashboardActionCard
      dataTestidFirstButton={
        voter?.isRegisteredAsDRep ? "retire-button" : "register-button"
      }
      dataTestidDrepIdBox="my-drep-id"
      firstButtonVariant={voter?.isRegisteredAsDRep ? "outlined" : "contained"}
      secondButtonVariant={
        pendingTransaction.registerAsDrep || pendingTransaction.retireAsDrep
          ? "outlined"
          : voter?.isRegisteredAsDRep
            ? "text"
            : "outlined"
      }
      dataTestidSecondButton={
        voter?.isRegisteredAsDRep
          ? "change-metadata-button"
          : "register-learn-more-button"
      }
      description={registrationCardDescription}
      firstButtonAction={
        voter?.isRegisteredAsDRep
          ? () => navigateTo(PATHS.retireAsDrep)
          : () => navigateTo(PATHS.registerAsdRep)
      }
      firstButtonLabel={
        pendingTransaction.registerAsDrep || pendingTransaction.retireAsDrep
          ? ""
          : t(
            `dashboard.registration.${
              voter?.isRegisteredAsDRep ? "retire" : "register"
            }`
          )
      }
      inProgress={
        !!(
          pendingTransaction.registerAsDrep ||
          pendingTransaction.retireAsDrep ||
          pendingTransaction.updateMetaData
        )
      }
      imageURL={IMAGES.govActionRegisterImage}
      secondButtonAction={
        pendingTransaction.registerAsDrep || pendingTransaction.retireAsDrep
          ? () => openInNewTab("https://adanordic.com/latest_transactions")
          : voter?.isRegisteredAsDRep
            ? () => {
              navigateTo(PATHS.updateMetadata);
            }
            : () =>
              openInNewTab(
                "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
              )
      }
      secondButtonLabel={
        pendingTransaction.registerAsDrep || pendingTransaction.retireAsDrep
          ? t("seeTransaction")
          : voter?.isRegisteredAsDRep
            ? t("dashboard.registration.changeMetadata")
            : t("learnMore")
      }
      cardId={
        voter?.isRegisteredAsDRep || voter?.wasRegisteredAsDRep
          ? dRepIDBech32
          : ""
      }
      cardTitle={
        voter?.isRegisteredAsDRep || voter?.wasRegisteredAsDRep
          ? t("myDRepId")
          : ""
      }
      title={registrationCardTitle}
    />
  );
};
