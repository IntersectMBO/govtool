import { useCallback, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { correctAdaFormat, formHexToBech32, openInNewTab } from "@utils";
import { PendingTransaction } from "@/context/pendingTransaction";

type DelegateDashboardCardProps = {
  currentDelegation: string;
  dRepID: string;
  isPendingTransaction: () => boolean;
  pendingTransaction: PendingTransaction;
  votingPower: number;
};

export const DelegateDashboardCard = ({
  currentDelegation,
  dRepID,
  isPendingTransaction,
  pendingTransaction,
  votingPower,
}: DelegateDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const delegationDescription = useMemo(() => {
    const correctAdaRepresentation = correctAdaFormat(votingPower);
    if (currentDelegation === dRepID) {
      return (
        <Trans
          i18nKey="dashboard.delegation.toYourself"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    } if (currentDelegation === "drep_always_no_confidence") {
      return (
        <Trans
          i18nKey="dashboard.delegation.voteNo"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    } if (currentDelegation === "drep_always_abstain") {
      return (
        <Trans
          i18nKey="dashboard.delegation.voteAbstain"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    } if (currentDelegation) {
      return (
        <Trans
          i18nKey="dashboard.delegation.toDRep"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    return (
      <Trans
        i18nKey="dashboard.delegation.delegateOwnPower"
        values={{ ada: correctAdaRepresentation }}
      />
    );
  }, [currentDelegation, dRepID, votingPower]);

  const delegationStatusTestForId = useMemo(() => {
    if (currentDelegation === dRepID) {
      return "myself";
    } if (currentDelegation === "drep_always_no_confidence") {
      return "no-confidence";
    } if (currentDelegation === "drep_always_abstain") {
      return "abstain";
    } if (currentDelegation) {
      return "dRep";
    }
    return "not_delegated";
  }, [currentDelegation, dRepID, votingPower]);

  const progressDescription = useMemo(() => {
    const correctAdaRepresentation = correctAdaFormat(votingPower);
    if (!pendingTransaction.delegate) return;
    const { resourceId } = pendingTransaction.delegate;
    if (resourceId === dRepID) {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.toYourself"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (resourceId === "no confidence") {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.voteNo"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (resourceId === "abstain") {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.voteAbstain"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (resourceId) {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.toDRep"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
  }, [pendingTransaction, dRepID, votingPower]);

  const navigateTo = useCallback(
    (path: string) => {
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;
      navigate(path);
    },
    [isPendingTransaction, navigate]
  );

  const displayedDelegationId = useMemo(() => {
    const restrictedNames = [
      dRepID,
      "drep_always_abstain",
      "drep_always_no_confidence",
      "abstain",
      "no confidence",
    ];
    if (pendingTransaction.delegate) {
      const delegateTo = pendingTransaction.delegate.resourceId;
      if (!restrictedNames.includes(delegateTo)) {
        return delegateTo.includes("drep")
          ? delegateTo
          : formHexToBech32(delegateTo);
      }
      return undefined;
    }
    if (!restrictedNames.includes(currentDelegation)) {
      return formHexToBech32(currentDelegation);
    }
    return undefined;
  }, [currentDelegation, dRepID, pendingTransaction, formHexToBech32]);

  return (
    <DashboardActionCard
      dataTestidFirstButton={
        currentDelegation ? "change-dRep-button" : "delegate-button"
      }
      dataTestidSecondButton="delegate-learn-more-button"
      dataTestidDrepIdBox="delegated-to-drep-id"
      description={
        pendingTransaction.delegate
          ? progressDescription
          : delegationDescription
      }
      dataTestidDelegationStatus={
        pendingTransaction.delegate
          ? "voting-power-delegation-status-in-progress"
          : `voting-power-delegation-status-${delegationStatusTestForId}`
      }
      firstButtonAction={() => navigateTo(PATHS.delegateTodRep)}
      firstButtonLabel={
        pendingTransaction.delegate
          ? ""
          : currentDelegation
            ? t("dashboard.delegation.changeDelegation")
            : t("delegate")
      }
      firstButtonVariant={currentDelegation ? "outlined" : "contained"}
      imageURL={IMAGES.govActionDelegateImage}
      cardId={displayedDelegationId}
      inProgress={!!pendingTransaction.delegate}
      cardTitle={t("dashboard.delegation.dRepDelegatedTo")}
      secondButtonAction={
        pendingTransaction.delegate
          ? () => openInNewTab("https://adanordic.com/latest_transactions")
          : () =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power"
            )
      }
      secondButtonLabel={
        pendingTransaction.delegate
          ? t("seeTransaction")
          : currentDelegation
            ? ""
            : t("learnMore")
      }
      title={
        pendingTransaction.delegate ? (
          t("dashboard.delegation.votingPowerDelegation")
        ) : currentDelegation ? (
          <Trans i18nKey="dashboard.delegation.yourVotingPowerIsDelegated" />
        ) : (
          t("dashboard.delegation.useYourVotingPower")
        )
      }
    />
  );
};
