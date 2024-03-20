import { useCallback, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { correctAdaFormat, openInNewTab } from "@utils";
import { PendingTransaction } from "@/context/pendingTransaction";
import { VoterInfo } from "@/models";

type SoleVoterDashboardCardProps = {
  isPendingTransaction: () => boolean;
  pendingTransaction: PendingTransaction;
  voter: VoterInfo;
  votingPower: number;
};

export const SoleVoterDashboardCard = ({
  isPendingTransaction,
  pendingTransaction,
  voter,
  votingPower,
}: SoleVoterDashboardCardProps) => {
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

  const soleVoterCardDescription = useMemo(() => {
    if (pendingTransaction.registerAsSoleVoter) return "dashboard.soleVoter.registrationInProgress";

    if (pendingTransaction.retireAsSoleVoter) return "dashboard.soleVoter.retirementInProgress";

    if (voter?.isRegisteredAsSoleVoter) return "dashboard.soleVoter.isRegisteredDescription";

    if (voter?.wasRegisteredAsSoleVoter) return "dashboard.soleVoter.wasRegisteredDescription";

    return "dashboard.soleVoter.registerDescription";
  }, [
    pendingTransaction,
    voter?.isRegisteredAsSoleVoter,
    voter?.wasRegisteredAsSoleVoter,
  ]);

  const soleVoterCardTitle = useMemo(() => {
    if (pendingTransaction.retireAsSoleVoter) return t("dashboard.soleVoter.retirement");

    if (pendingTransaction.registerAsSoleVoter) return t("dashboard.soleVoter.registration");

    if (voter?.isRegisteredAsSoleVoter) return t("dashboard.soleVoter.youAreSoleVoterTitle");

    if (voter?.wasRegisteredAsSoleVoter) return t("dashboard.soleVoter.wasSoleVoterTitle");

    return t("dashboard.soleVoter.registerTitle");
  }, [
    pendingTransaction,
    voter?.isRegisteredAsSoleVoter,
    voter?.isRegisteredAsSoleVoter,
  ]);

  return (
    <DashboardActionCard
      title={soleVoterCardTitle}
      inProgress={
        !!pendingTransaction.registerAsSoleVoter ||
        !!pendingTransaction.retireAsSoleVoter
      }
      dataTestidFirstButton={
        voter?.isRegisteredAsSoleVoter
          ? "retire-as-sole-voter-button"
          : "register-as-sole-voter-button"
      }
      dataTestidSecondButton="learn-more-button"
      description={(
        <Trans
          i18nKey={soleVoterCardDescription}
          values={{ votingPower: correctAdaFormat(votingPower) }}
        />
      )}
      firstButtonLabel={
        pendingTransaction.registerAsSoleVoter
          ? ""
          : t(
            voter?.isRegisteredAsSoleVoter
              ? "dashboard.soleVoter.retire"
              : voter?.wasRegisteredAsSoleVoter
                ? "dashboard.soleVoter.reRegister"
                : "dashboard.soleVoter.register"
          )
      }
      firstButtonAction={() =>
        navigateTo(
          voter?.isRegisteredAsSoleVoter
            ? PATHS.retireAsSoleVoter
            : PATHS.registerAsSoleVoter
        )}
      firstButtonVariant={
        voter?.isRegisteredAsSoleVoter ? "outlined" : "contained"
      }
      secondButtonLabel={t("learnMore")}
      secondButtonAction={() =>
        openInNewTab(
          "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
        )}
      secondButtonVariant="outlined"
      imageURL={IMAGES.soleVoterImage}
    />
  );
};
