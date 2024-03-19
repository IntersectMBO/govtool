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

export function DashboardCards() {
  const {
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    delegateTo,
    delegateTransaction,
    dRepID,
    dRepIDBech32,
    govActionTransaction,
    isDrepLoading,
    isPendingTransaction,
    registerTransaction,
    soleVoterTransaction,
    stakeKey,
    voter,
  } = useCardano();
  const navigate = useNavigate();
  const { currentDelegation, isCurrentDelegationLoading } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const [isRetirementLoading, setIsRetirementLoading] = useState<boolean>(false);
  const { votingPower, powerIsLoading } = useGetAdaHolderVotingPowerQuery(stakeKey);
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
      if (result) {
        openModal({
          type: "statusModal",
          state: {
            status: "success",
            title: t("modals.retirement.title"),
            message: t("modals.retirement.message"),
            link: "https://adanordic.com/latest_transactions",
            buttonText: t("modals.common.goToDashboard"),
            dataTestId: "retirement-transaction-submitted-modal",
          },
        });
      }
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
    [isPendingTransaction, navigate],
  );

  const onClickGovernanceActionCardActionButton = useCallback(() => {
    if (govActionTransaction.transactionHash) {
      navigate(PATHS.dashboardGovernanceActions);
      return;
    }
    navigate(PATHS.createGovernanceAction);
  }, [govActionTransaction.transactionHash, navigate]);

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
    }
    return undefined;
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
    } else if (voter?.isRegisteredAsDRep || voter?.wasRegisteredAsDRep) {
      return t("dashboard.registration.holdersCanDelegate");
    } else {
      return t("dashboard.registration.ifYouWant");
    }
  }, [
    registerTransaction.transactionHash,
    registerTransaction.type,
    voter?.isRegisteredAsDRep,
    voter?.wasRegisteredAsDRep,
  ]);

  const soleVoterCardDescription = useMemo(() => {
    if (soleVoterTransaction.transactionHash) {
      switch (soleVoterTransaction.type) {
        case "retirement":
          return "dashboard.soleVoter.retirementInProgress";
        default:
          return "dashboard.soleVoter.registrationInProgress";
      }
    } else if (voter?.isRegisteredAsSoleVoter) {
      return "dashboard.soleVoter.isRegisteredDescription";
    } else if (voter?.wasRegisteredAsSoleVoter) {
      return "dashboard.soleVoter.wasRegisteredDescription";
    } else {
      return "dashboard.soleVoter.registerDescription";
    }
  }, [
    soleVoterTransaction.transactionHash,
    soleVoterTransaction.type,
    voter?.isRegisteredAsSoleVoter,
    voter?.wasRegisteredAsSoleVoter,
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
    } else if (voter?.isRegisteredAsDRep) {
      return t("dashboard.registration.youAreRegistered");
    } else if (voter?.wasRegisteredAsDRep) {
      return t("dashboard.registration.registerAgain");
    } else {
      return t("dashboard.registration.registerAsDRep");
    }
  }, [
    registerTransaction?.transactionHash,
    registerTransaction.type,
    voter?.isRegisteredAsDRep,
    voter?.wasRegisteredAsDRep,
  ]);

  const soleVoterCardTitle = useMemo(() => {
    if (soleVoterTransaction?.transactionHash) {
      switch (soleVoterTransaction.type) {
        case "retirement":
          return t("dashboard.soleVoter.retirement");
        default:
          return t("dashboard.soleVoter.registration");
      }
    } else if (voter?.isRegisteredAsSoleVoter) {
      return t("dashboard.soleVoter.youAreSoleVoterTitle");
    } else if (voter?.wasRegisteredAsSoleVoter) {
      return t("dashboard.soleVoter.wasSoleVoterTitle");
    } else {
      return t("dashboard.soleVoter.registerTitle");
    }
  }, [
    soleVoterTransaction?.transactionHash,
    soleVoterTransaction.type,
    voter?.isRegisteredAsSoleVoter,
    voter?.isRegisteredAsSoleVoter,
  ]);

  return isDrepLoading ? (
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
            : () => openInNewTab(
              "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power",
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
          registerTransaction?.transactionHash
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
          registerTransaction?.transactionHash
            ? ""
            : t(
              `dashboard.registration.${
                voter?.isRegisteredAsDRep ? "retire" : "register"
              }`,
            )
        }
        inProgress={!!registerTransaction?.transactionHash}
        imageURL={IMAGES.govActionRegisterImage}
        secondButtonAction={
          registerTransaction?.transactionHash
            ? () => openInNewTab("https://adanordic.com/latest_transactions")
            : voter?.isRegisteredAsDRep
              ? () => {
                navigateTo(PATHS.updateMetadata);
              }
              : () => openInNewTab(
                "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep",
              )
        }
        secondButtonLabel={
          registerTransaction?.transactionHash
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
        inProgress={!!soleVoterTransaction?.transactionHash}
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
          soleVoterTransaction?.transactionHash
            ? ""
            : t(
              voter?.isRegisteredAsSoleVoter
                ? "dashboard.soleVoter.retire"
                : voter?.wasRegisteredAsSoleVoter
                  ? "dashboard.soleVoter.reRegister"
                  : "dashboard.soleVoter.register",
            )
        }
        firstButtonAction={() => navigateTo(
          voter?.isRegisteredAsSoleVoter
            ? PATHS.retireAsSoleVoter
            : PATHS.registerAsSoleVoter,
        )}
        firstButtonVariant={
          voter?.isRegisteredAsSoleVoter ? "outlined" : "contained"
        }
        secondButtonLabel={t("learnMore")}
        secondButtonAction={() => openInNewTab(
          "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep",
        )}
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
            govActionTransaction.transactionHash ? "view" : "propose"
          }`,
        )}
        inProgress={!!govActionTransaction.transactionHash}
        secondButtonLabel={t("learnMore")}
        secondButtonAction={() => openInNewTab(
          "https://docs.sanchogov.tools/faqs/what-is-a-governance-action",
        )}
        secondButtonVariant="outlined"
        imageURL={IMAGES.proposeGovActionImage}
        title={t("dashboard.proposeGovernanceAction.title")}
      />
      {/* GOV ACTIONS LIST CARD END */}
    </Box>
  );
}
