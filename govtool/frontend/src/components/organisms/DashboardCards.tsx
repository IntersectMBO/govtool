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
  useGetVoterInfo,
} from "@hooks";
import { DashboardActionCard } from "@molecules";
import { correctAdaFormat, formHexToBech32, openInNewTab } from "@utils";

export const DashboardCards = () => {
  const {
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    dRepID,
    dRepIDBech32,
    isPendingTransaction,
    pendingTransaction,
    stakeKey,
  } = useCardano();
  const navigate = useNavigate();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const [isRetirementLoading, setIsRetirementLoading] =
    useState<boolean>(false);
  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();

  const retireAsDrep = useCallback(async () => {
    try {
      setIsRetirementLoading(true);
      const isPendingTx = isPendingTransaction();

      if (isPendingTx) return;
      if (!voter?.deposit) throw new Error("Can not get deposit");

      const certBuilder = await buildDRepRetirementCert(
        voter.deposit.toString(),
      );
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "retireAsDrep",
        voterDeposit: voter.deposit.toString(),
      });
      if (result) {
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
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
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
    voter?.deposit,
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
    }
    if (currentDelegation === "drep_always_no_confidence") {
      return (
        <Trans
          i18nKey="dashboard.delegation.voteNo"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (currentDelegation === "drep_always_abstain") {
      return (
        <Trans
          i18nKey="dashboard.delegation.voteAbstain"
          values={{ ada: correctAdaRepresentation }}
        />
      );
    }
    if (currentDelegation) {
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
    }
    if (currentDelegation === "drep_always_no_confidence") {
      return "no-confidence";
    }
    if (currentDelegation === "drep_always_abstain") {
      return "abstain";
    }
    if (currentDelegation) {
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
    [isPendingTransaction, navigate],
  );

  const onClickGovernanceActionCardActionButton = useCallback(() => {
    if (pendingTransaction.createGovAction) {
      navigate(PATHS.dashboardGovernanceActions);
      return;
    }
    navigate(PATHS.createGovernanceAction);
  }, [pendingTransaction.createGovAction, navigate]);

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

  const registrationCardDescription = useMemo(() => {
    if (pendingTransaction.registerAsDrep)
      return t("dashboard.registration.registrationInProgress");

    if (pendingTransaction.retireAsDrep)
      return t("dashboard.registration.retirementInProgress");

    if (pendingTransaction.updateMetaData)
      return t("dashboard.registration.metadataUpdateInProgress");

    if (voter?.isRegisteredAsDRep || voter?.wasRegisteredAsDRep)
      return t("dashboard.registration.holdersCanDelegate");

    return t("dashboard.registration.ifYouWant");
  }, [
    pendingTransaction,
    voter?.isRegisteredAsDRep,
    voter?.wasRegisteredAsDRep,
  ]);

  const soleVoterCardDescription = useMemo(() => {
    if (pendingTransaction.registerAsSoleVoter)
      return "dashboard.soleVoter.registrationInProgress";

    if (pendingTransaction.retireAsSoleVoter)
      return "dashboard.soleVoter.retirementInProgress";

    if (voter?.isRegisteredAsSoleVoter)
      return "dashboard.soleVoter.isRegisteredDescription";

    if (voter?.wasRegisteredAsSoleVoter)
      return "dashboard.soleVoter.wasRegisteredDescription";

    return "dashboard.soleVoter.registerDescription";
  }, [
    pendingTransaction,
    voter?.isRegisteredAsSoleVoter,
    voter?.wasRegisteredAsSoleVoter,
  ]);

  const registrationCardTitle = useMemo(() => {
    if (pendingTransaction.retireAsDrep)
      return t("dashboard.registration.dRepRetirement");

    if (pendingTransaction.registerAsDrep)
      return t("dashboard.registration.dRepRegistration");

    if (pendingTransaction.updateMetaData)
      return t("dashboard.registration.dRepUpdate");

    if (voter?.isRegisteredAsDRep)
      return t("dashboard.registration.youAreRegistered");

    if (voter?.wasRegisteredAsDRep)
      return t("dashboard.registration.registerAgain");

    return t("dashboard.registration.registerAsDRep");
  }, [
    pendingTransaction,
    voter?.isRegisteredAsDRep,
    voter?.wasRegisteredAsDRep,
  ]);

  const soleVoterCardTitle = useMemo(() => {
    if (pendingTransaction.retireAsSoleVoter)
      return t("dashboard.soleVoter.retirement");

    if (pendingTransaction.registerAsSoleVoter)
      return t("dashboard.soleVoter.registration");

    if (voter?.isRegisteredAsSoleVoter)
      return t("dashboard.soleVoter.youAreSoleVoterTitle");

    if (voter?.wasRegisteredAsSoleVoter)
      return t("dashboard.soleVoter.wasSoleVoterTitle");

    return t("dashboard.soleVoter.registerTitle");
  }, [
    pendingTransaction,
    voter?.isRegisteredAsSoleVoter,
    voter?.isRegisteredAsSoleVoter,
  ]);

  return !voter || !votingPower ? (
    <Box
      sx={{
        alignItems: "center",
        display: "flex",
        flex: 1,
        height: "100vh",
        justifyContent: "center",
      }}
    >
      <CircularProgress />
    </Box>
  ) : (
    <Box
      sx={{
        columnGap: 3,
        display: "grid",
        gridTemplateColumns:
          screenWidth < 1280
            ? "repeat(1, minmax(300px, 530px))"
            : screenWidth >= 1728
            ? "repeat(3, minmax(300px, 570px))"
            : "repeat(2, minmax(300px, 530px))",
        justifyContent: screenWidth < 1024 ? "center" : "flex-start",
        px: screenWidth < 640 ? 2 : 5,
        py: 3,
        rowGap: 3,
      }}
    >
      {/* DELEGATION CARD */}
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
                  "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power",
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
      {/* DELEGATION CARD END */}
      {/* REGISTARTION AS DREP CARD */}
      <DashboardActionCard
        dataTestidFirstButton={
          voter?.isRegisteredAsDRep ? "retire-button" : "register-button"
        }
        dataTestidDrepIdBox="my-drep-id"
        firstButtonVariant={
          voter?.isRegisteredAsDRep ? "outlined" : "contained"
        }
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
            ? retireAsDrep
            : () => navigateTo(PATHS.registerAsdRep)
        }
        firstButtonIsLoading={isRetirementLoading}
        firstButtonLabel={
          pendingTransaction.registerAsDrep || pendingTransaction.retireAsDrep
            ? ""
            : t(
                `dashboard.registration.${
                  voter?.isRegisteredAsDRep ? "retire" : "register"
                }`,
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
                  "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep",
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
      {/* DREP CARD END */}
      {/* SOLE VOTER CARD */}
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
        description={
          <Trans
            i18nKey={soleVoterCardDescription}
            values={{ votingPower: correctAdaFormat(votingPower) }}
          />
        }
        firstButtonLabel={
          pendingTransaction.registerAsSoleVoter
            ? ""
            : t(
                voter?.isRegisteredAsSoleVoter
                  ? "dashboard.soleVoter.retire"
                  : voter?.wasRegisteredAsSoleVoter
                  ? "dashboard.soleVoter.reRegister"
                  : "dashboard.soleVoter.register",
              )
        }
        firstButtonAction={() =>
          navigateTo(
            voter?.isRegisteredAsSoleVoter
              ? PATHS.retireAsSoleVoter
              : PATHS.registerAsSoleVoter,
          )
        }
        firstButtonVariant={
          voter?.isRegisteredAsSoleVoter ? "outlined" : "contained"
        }
        secondButtonLabel={t("learnMore")}
        secondButtonAction={() =>
          openInNewTab(
            "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep",
          )
        }
        secondButtonVariant="outlined"
        imageURL={IMAGES.soleVoterImage}
      />
      {/* REGISTARTION AS SOLE VOTER CARD END */}
      {/* GOV ACTIONS LIST CARD */}
      <DashboardActionCard
        dataTestidFirstButton="view-governance-actions-button"
        description={t("dashboard.govActions.description")}
        firstButtonAction={() => navigate(PATHS.dashboardGovernanceActions)}
        firstButtonLabel={t(
          `dashboard.govActions.${
            voter?.isRegisteredAsDRep ? "reviewAndVote" : "view"
          }`,
        )}
        imageURL={IMAGES.govActionListImage}
        title={t("dashboard.govActions.title")}
      />
      {/* GOV ACTIONS LIST CARD END */}
      {/* GOV ACTIONS LIST CARD */}
      <DashboardActionCard
        dataTestidFirstButton="propose-governance-actions-button"
        description={t("dashboard.proposeGovernanceAction.description")}
        firstButtonAction={onClickGovernanceActionCardActionButton}
        firstButtonLabel={t(
          `dashboard.proposeGovernanceAction.${
            pendingTransaction.createGovAction ? "view" : "propose"
          }`,
        )}
        inProgress={!!pendingTransaction.createGovAction}
        secondButtonLabel={t("learnMore")}
        secondButtonAction={() =>
          openInNewTab(
            "https://docs.sanchogov.tools/faqs/what-is-a-governance-action",
          )
        }
        secondButtonVariant="outlined"
        imageURL={IMAGES.proposeGovActionImage}
        title={t("dashboard.proposeGovernanceAction.title")}
      />
      {/* GOV ACTIONS LIST CARD END */}
    </Box>
  );
};
