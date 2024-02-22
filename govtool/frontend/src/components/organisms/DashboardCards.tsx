import { useCallback, useMemo, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Box, CircularProgress } from "@mui/material";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
  useGetAdaHolderCurrentDelegationQuery,
  useTranslation,
} from "@hooks";
import { DashboardActionCard } from "@molecules";
import { correctAdaFormat, formHexToBech32, openInNewTab } from "@utils";

export const DashboardCards = () => {
  const {
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    delegateTo,
    delegateTransaction,
    user,
    dRepID,
    dRepIDBech32,
    isDrepLoading,
    isPendingTransaction,
    registerTransaction,
    soleVoterTransaction,
    stakeKey,
  } = useCardano();
  const navigate = useNavigate();
  const { currentDelegation, isCurrentDelegationLoading } =
    useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const [isRetirementLoading, setIsRetirementLoading] =
    useState<boolean>(false);
  const { votingPower, powerIsLoading } =
    useGetAdaHolderVotingPowerQuery(stakeKey);
  const { t } = useTranslation();

  const retireAsDrep = useCallback(async () => {
    try {
      setIsRetirementLoading(true);
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;
      const certBuilder = await buildDRepRetirementCert();
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "registration",
        registrationType: "retirement",
      });
      if (result)
        openModal({
          type: "statusModal",
          state: {
            status: "success",
            title: t("modals.retirement.title"),
            message: t("modals.retirement.message"),
            link: `https://adanordic.com/latest_transactions`,
            buttonText: t("modals.common.goToDashboard"),
            dataTestId: "retirement-transaction-submitted-modal",
          },
        });
    } catch (error: any) {
      const errorMessage = error.info ? error.info : error;

      openModal({
        type: "statusModal",
        state: {
          status: "warning",
          message: errorMessage,
          buttonText: t("modals.common.goToDashboard"),
          title: t("modals.common.oops"),
          dataTestId: "retirement-transaction-error-modal",
        },
      });
    } finally {
      setIsRetirementLoading(false);
    }
  }, [
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    isPendingTransaction,
    openModal,
  ]);

  const delegationDescription = useMemo(() => {
    const correctAdaRepresentation = correctAdaFormat(votingPower);
    if (currentDelegation === dRepID) {
      return (
        <Trans
          i18nKey="dashboard.delegation.toYourself"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    } else if (currentDelegation === "drep_always_no_confidence") {
      return (
        <Trans
          i18nKey="dashboard.delegation.voteNo"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    } else if (currentDelegation === "drep_always_abstain") {
      return (
        <Trans
          i18nKey="dashboard.delegation.voteAbstain"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    } else if (currentDelegation) {
      return (
        <Trans
          i18nKey="dashboard.delegation.toDRep"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    } else {
      return (
        <Trans
          i18nKey="dashboard.delegation.delegateOwnPower"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
  }, [currentDelegation, dRepID, votingPower]);

  const delegationStatusTestForId = useMemo(() => {
    if (currentDelegation === dRepID) {
      return "myself";
    } else if (currentDelegation === "drep_always_no_confidence") {
      return "no-confidence";
    } else if (currentDelegation === "drep_always_abstain") {
      return "abstain";
    } else if (currentDelegation) {
      return "dRep";
    } else {
      return "not_delegated";
    }
  }, [currentDelegation, dRepID, votingPower]);

  const progressDescription = useMemo(() => {
    const correctAdaRepresentation = correctAdaFormat(votingPower);
    if (delegateTo === dRepID) {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.toYourself"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (delegateTo === "no confidence") {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.voteNo"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (delegateTo === "abstain") {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.voteAbstain"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (delegateTo) {
      return (
        <Trans
          i18nKey="dashboard.delegation.inProgress.toDRep"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
  }, [delegateTo, dRepID, votingPower]);

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
    if (delegateTransaction?.transactionHash) {
      if (!restrictedNames.includes(delegateTo)) {
        return delegateTo.includes("drep")
          ? delegateTo
          : formHexToBech32(delegateTo);
      }
      return undefined;
    }
    if (!restrictedNames.includes(currentDelegation)) {
      return formHexToBech32(currentDelegation);
    } else {
      return undefined;
    }
  }, [
    currentDelegation,
    dRepID,
    delegateTo,
    delegateTransaction,
    formHexToBech32,
  ]);

  const registrationCardDescription = useMemo(() => {
    if (registerTransaction.transactionHash) {
      switch (registerTransaction.type) {
        case "retirement":
          return t("dashboard.registration.retirementInProgress");
        case "registration":
          return t("dashboard.registration.registrationInProgress");
        default:
          return t("dashboard.registration.metadataUpdateInProgress");
      }
    } else if (user?.isRegisteredAsDRep || user?.wasRegisteredAsDRep) {
      return t("dashboard.registration.holdersCanDelegate");
    } else {
      return t("dashboard.registration.ifYouWant");
    }
  }, [
    registerTransaction.transactionHash,
    registerTransaction.type,
    user?.isRegisteredAsDRep,
    user?.wasRegisteredAsDRep,
  ]);

  const registrationCardTitle = useMemo(() => {
    if (registerTransaction?.transactionHash) {
      switch (registerTransaction.type) {
        case "retirement":
          return t("dashboard.registration.dRepRetirement");
        case "registration":
          return t("dashboard.registration.dRepRegistration");
        default:
          return t("dashboard.registration.dRepUpdate");
      }
    } else if (user?.isRegisteredAsDRep) {
      return t("dashboard.registration.youAreRegistered");
    } else if (user?.wasRegisteredAsDRep) {
      return t("dashboard.registration.registerAgain");
    } else {
      return t("dashboard.registration.registerAsDRep");
    }
  }, [
    registerTransaction?.transactionHash,
    registerTransaction.type,
    user?.isRegisteredAsDRep,
    user?.wasRegisteredAsDRep,
  ]);

  return isDrepLoading ? (
    <Box
      alignItems="center"
      display="flex"
      flex={1}
      height="100vh"
      justifyContent="center"
    >
      <CircularProgress />
    </Box>
  ) : (
    <Box
      columnGap={3}
      display="grid"
      gridTemplateColumns={screenWidth < 1024 ? "1fr" : "1fr 1fr"}
      px={screenWidth < 1024 ? 2 : screenWidth < 1440 ? 5 : 4}
      py={3}
      rowGap={4}
    >
      {/* DELEGATION CARD */}
      <DashboardActionCard
        dataTestidFirstButton={
          currentDelegation ? "change-dRep-button" : "delegate-button"
        }
        dataTestidSecondButton="delegate-learn-more-button"
        dataTestidDrepIdBox="delegated-to-drep-id"
        isLoading={isCurrentDelegationLoading || powerIsLoading}
        description={
          delegateTransaction?.transactionHash
            ? progressDescription
            : delegationDescription
        }
        dataTestidDelegationStatus={
          delegateTransaction?.transactionHash
            ? "voting-power-delegation-status-in-progress"
            : `voting-power-delegation-status-${delegationStatusTestForId}`
        }
        firstButtonAction={() => navigateTo(PATHS.delegateTodRep)}
        firstButtonLabel={
          delegateTransaction?.transactionHash
            ? ""
            : currentDelegation
            ? t("dashboard.delegation.changeDelegation")
            : t("delegate")
        }
        firstButtonVariant={currentDelegation ? "outlined" : "contained"}
        imageURL={IMAGES.govActionDelegateImage}
        cardId={displayedDelegationId}
        inProgress={!!delegateTransaction?.transactionHash}
        cardTitle={t("dashboard.delegation.dRepDelegatedTo")}
        secondButtonAction={
          delegateTransaction?.transactionHash
            ? () => openInNewTab("https://adanordic.com/latest_transactions")
            : () =>
                openInNewTab(
                  "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power"
                )
        }
        secondButtonLabel={
          delegateTransaction?.transactionHash
            ? t("seeTransaction")
            : currentDelegation
            ? ""
            : t("learnMore")
        }
        title={
          delegateTransaction?.transactionHash ? (
            t("dashboard.delegation.votingPowerDelegation")
          ) : currentDelegation ? (
            <Trans i18nKey="dashboard.delegation.yourVotingPowerIsDelegated" />
          ) : (
            t("dashboard.delegation.useYourVotingPower")
          )
        }
      />
      {/* DELEGATION CARD END*/}
      {/* REGISTARTION AS DREP CARD */}
      <DashboardActionCard
        dataTestidFirstButton={
          user?.isRegisteredAsDRep ? "retire-button" : "register-button"
        }
        dataTestidDrepIdBox="my-drep-id"
        firstButtonVariant={user?.isRegisteredAsDRep ? "outlined" : "contained"}
        secondButtonVariant={
          registerTransaction?.transactionHash
            ? "outlined"
            : user?.isRegisteredAsDRep
            ? "text"
            : "outlined"
        }
        dataTestidSecondButton={
          user?.isRegisteredAsDRep
            ? "change-metadata-button"
            : "register-learn-more-button"
        }
        description={registrationCardDescription}
        firstButtonAction={
          user?.isRegisteredAsDRep
            ? retireAsDrep
            : () => navigateTo(PATHS.registerAsdRep)
        }
        firstButtonIsLoading={isRetirementLoading}
        firstButtonLabel={
          registerTransaction?.transactionHash
            ? ""
            : t(
                `dashboard.registration.${
                  user?.isRegisteredAsDRep ? "retire" : "register"
                }`
              )
        }
        inProgress={!!registerTransaction?.transactionHash}
        imageURL={IMAGES.govActionRegisterImage}
        secondButtonAction={
          registerTransaction?.transactionHash
            ? () => openInNewTab("https://adanordic.com/latest_transactions")
            : user?.isRegisteredAsDRep
            ? () => {
                navigateTo(PATHS.updateMetadata);
              }
            : () =>
                openInNewTab(
                  "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
                )
        }
        secondButtonLabel={
          registerTransaction?.transactionHash
            ? t("seeTransaction")
            : user?.isRegisteredAsDRep
            ? t("dashboard.registration.changeMetadata")
            : t("learnMore")
        }
        cardId={user?.isRegisteredAsDRep || user?.wasRegisteredAsDRep ? dRepIDBech32 : ""}
        cardTitle={
          user?.isRegisteredAsDRep || user?.wasRegisteredAsDRep ? t("myDRepId") : ""
        }
        title={registrationCardTitle}
      />
      {/* DREP CARD END*/}
      {/* SOLE VOTER CARD */}
      <DashboardActionCard
        dataTestidFirstButton={
          user?.isRegisteredAsSoleVoter
            ? "retire-as-sole-voter-button"
            : "register-as-sole-voter-button"
        }
        dataTestidSecondButton="learn-more-button"
        description={
          <Trans
            i18nKey={
              user?.isRegisteredAsSoleVoter
                ? "dashboard.soleVoter.youAreSoleVoterDescription"
                : "dashboard.soleVoter.registerAsSoleVoterDescription"
            }
            values={{ votingPower: correctAdaFormat(votingPower) }}
          />
        }
        firstButtonAction={() =>
          navigateTo(
            user?.isRegisteredAsSoleVoter
              ? PATHS.retireAsSoleVoter
              : PATHS.registerAsSoleVoter
          )
        }
        firstButtonLabel={t(
          user?.isRegisteredAsSoleVoter
            ? "dashboard.soleVoter.retire"
            : "dashboard.soleVoter.register"
        )}
        firstButtonVariant={user?.isRegisteredAsSoleVoter ? "outlined" : "contained"}
        secondButtonVariant={
          soleVoterTransaction?.transactionHash
            ? "outlined"
            : user?.isRegisteredAsSoleVoter
            ? "text"
            : "outlined"
        }
        inProgress={!!soleVoterTransaction?.transactionHash}
        imageURL={IMAGES.soleVoterImage}
        secondButtonAction={() =>
          openInNewTab(
            "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
          )
        }
        secondButtonLabel={t("learnMore")}
        title={t(
          user?.isRegisteredAsSoleVoter
            ? "dashboard.soleVoter.youAreSoleVoterTitle"
            : "dashboard.soleVoter.registerAsSoleVoterTitle"
        )}
      />
      {/* REGISTARTION AS SOLE VOTER CARD END*/}
      {/* GOV ACTIONS LIST CARD */}
      <DashboardActionCard
        dataTestidFirstButton="view-governance-actions-button"
        description={t("dashboard.govActions.description")}
        firstButtonAction={() => navigate(PATHS.dashboard_governance_actions)}
        firstButtonLabel={t(
          `dashboard.govActions.${
            user?.isRegisteredAsDRep ? "reviewAndVote" : "view"
          }`
        )}
        imageURL={IMAGES.govActionListImage}
        title={t("dashboard.govActions.title")}
      />
      {/* GOV ACTIONS LIST CARD END*/}
    </Box>
  );
};
