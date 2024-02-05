import { useNavigate } from "react-router-dom";
import { Box, CircularProgress, Typography } from "@mui/material";

import { IMAGES, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useGetAdaHolderVotingPowerQuery, useScreenDimension } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { useCallback, useMemo, useState } from "react";
import { useGetAdaHolderCurrentDelegationQuery } from "@hooks";
import { correctAdaFormat, formHexToBech32, openInNewTab } from "@utils";

export const DashboardCards = () => {
  const {
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    delegateTo,
    delegateTransaction,
    dRep,
    dRepID,
    dRepIDBech32,
    isDrepLoading,
    isPendingTransaction,
    registerTransaction,
    stakeKey,
  } = useCardano();
  const navigate = useNavigate();
  const { currentDelegation, isCurrentDelegationLoading } =
    useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { screenWidth, isMobile } = useScreenDimension();
  const { openModal } = useModal();
  const [isRetirementLoading, setIsRetirementLoading] =
    useState<boolean>(false);
  const { votingPower, powerIsLoading } =
    useGetAdaHolderVotingPowerQuery(stakeKey);

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
            title: "Retirement Transaction Submitted!",
            message:
              "The confirmation of your retirement might take a bit of time but you can track it using.",
            link: `https://adanordic.com/latest_transactions`,
            buttonText: "Go to dashboard",
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
          buttonText: "Go to dashboard",
          title: "Oops!",
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
    const correctAdaRepresentation = (
      <strong>{correctAdaFormat(votingPower)}</strong>
    );
    if (currentDelegation === dRepID) {
      return (
        <>
          You have delegated your voting power of ₳{correctAdaRepresentation} to
          yourself.
        </>
      );
    } else if (currentDelegation === "drep_always_no_confidence") {
      return (
        <>
          You have delegated your voting power of ₳{correctAdaRepresentation}.
          You are going to vote 'NO' as default.
        </>
      );
    } else if (currentDelegation === "drep_always_abstain") {
      return (
        <>
          You have delegated your voting power of ₳{correctAdaRepresentation}.
          You are going to vote 'ABSTAIN' as default.
        </>
      );
    } else if (currentDelegation) {
      return (
        <>
          You have delegated your voting power of ₳{correctAdaRepresentation} to
          a selected DRep.
        </>
      );
    } else {
      return (
        <>
          If you want to delegate your own voting power of ₳
          {correctAdaRepresentation}.
        </>
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
    const correctAdaRepresentation = (
      <strong>{correctAdaFormat(votingPower)}</strong>
    );
    if (delegateTo === dRepID) {
      return (
        <>
          Your own voting power of ₳{correctAdaRepresentation} is in progress of
          being delegated. You are going to delegate your voting power to
          yourself.
        </>
      );
    }
    if (delegateTo === "no confidence") {
      return (
        <>
          Your own voting power of ₳{correctAdaRepresentation} is in progress of
          being delegated. You are going to vote ‘NO’ as default.
        </>
      );
    }
    if (delegateTo === "abstain") {
      return (
        <>
          Your own voting power of ₳{correctAdaRepresentation} is in progress of
          being delegated. You are going to vote ‘ABSTAIN’ as default.
        </>
      );
    }
    if (delegateTo) {
      return (
        <>
          Your own voting power of ₳{correctAdaRepresentation} is progress of
          being delegated. You are going to delegate your voting power to a
          selected DRep.
        </>
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

  const renderGovActionSection = useCallback(() => {
    return (
      <>
        <Typography fontSize={16} fontWeight={600} lineHeight={"24px"} my={3}>
          See Active Governance Actions
        </Typography>
        <Box display={"flex"}>
          <DashboardActionCard
            dataTestidFirstButton="view-governance-actions-button"
            description="Review governance actions submitted on-chain."
            firstButtonAction={() =>
              navigate(PATHS.dashboard_governance_actions)
            }
            firstButtonLabel={
              dRep?.isRegistered ? "Review and vote" : "View governance actions"
            }
            imageURL={IMAGES.govActionListImage}
            title="View Governance Actions"
          />
          {screenWidth < 1024 ? null : (
            <>
              <Box p={2} />
              <Box flex={1} px={4} />
            </>
          )}
        </Box>
      </>
    );
  }, [screenWidth, isMobile, dRep?.isRegistered]);

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
      pl={screenWidth < 1024 ? 2 : screenWidth < 1440 ? 4 : 4}
      pr={screenWidth < 1024 ? 2 : screenWidth < 1440 ? 8 : 29.5}
    >
      {dRep?.isRegistered && renderGovActionSection()}
      <Typography fontSize={16} fontWeight={600} lineHeight={"24px"} my={3}>
        Your Participation
      </Typography>
      <Box
        display={"flex"}
        flexDirection={screenWidth < 1024 ? "column" : "row"}
      >
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
              ? "Change delegation"
              : "Delegate"
          }
          imageHeight={55}
          imageWidth={65}
          firstButtonVariant={currentDelegation ? "outlined" : "contained"}
          imageURL={IMAGES.govActionDelegateImage}
          cardId={displayedDelegationId}
          inProgress={!!delegateTransaction?.transactionHash}
          cardTitle="DRep you delegated to"
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
              ? "See transaction"
              : currentDelegation
              ? ""
              : "Learn more"
          }
          title={
            delegateTransaction?.transactionHash ? (
              "Voting Power Delegation"
            ) : currentDelegation ? (
              <>
                Your Voting Power <strong>is Delegated</strong>
              </>
            ) : (
              "Use your Voting Power"
            )
          }
        />
        <Box width={24} />
        <DashboardActionCard
          dataTestidFirstButton={
            dRep?.isRegistered ? "retire-button" : "register-button"
          }
          dataTestidDrepIdBox="my-drep-id"
          firstButtonVariant={dRep?.isRegistered ? "outlined" : "contained"}
          secondButtonVariant={
            registerTransaction?.transactionHash
              ? "outlined"
              : dRep?.isRegistered
              ? "text"
              : "outlined"
          }
          dataTestidSecondButton={
            dRep?.isRegistered
              ? "change-metadata-button"
              : "register-learn-more-button"
          }
          description={
            registerTransaction.transactionHash
              ? registerTransaction?.type === "retirement"
                ? "The retirement process is ongoing. This may take several minutes."
                : registerTransaction?.type === "registration"
                ? "The registration process is ongoing. This may take several minutes."
                : "The update DRep metadata is ongoing. This may take several minutes."
              : dRep?.isRegistered
              ? "Ada holders can delegate their voting power to you."
              : dRep?.wasRegistered
              ? "Ada holders can delegate their voting power to you."
              : "If you want to directly participate in voting and have other ada holders delegate their voting power to you."
          }
          firstButtonAction={
            dRep?.isRegistered
              ? retireAsDrep
              : () => navigateTo(PATHS.registerAsdRep)
          }
          firstButtonIsLoading={isRetirementLoading}
          firstButtonLabel={
            registerTransaction?.transactionHash
              ? ""
              : dRep?.isRegistered
              ? "Retire as a DRep"
              : "Register"
          }
          inProgress={!!registerTransaction?.transactionHash}
          imageURL={IMAGES.govActionRegisterImage}
          secondButtonAction={
            registerTransaction?.transactionHash
              ? () => openInNewTab("https://adanordic.com/latest_transactions")
              : dRep?.isRegistered
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
              ? "See transaction"
              : dRep?.isRegistered
              ? "Change metadata"
              : "Learn more"
          }
          cardId={dRep?.isRegistered || dRep?.wasRegistered ? dRepIDBech32 : ""}
          cardTitle={
            dRep?.isRegistered || dRep?.wasRegistered ? "My DRep ID" : ""
          }
          title={
            registerTransaction?.transactionHash
              ? registerTransaction?.type === "retirement"
                ? "DRep Retirement"
                : registerTransaction?.type === "registration"
                ? "DRep Registration"
                : "DRep Update"
              : dRep?.isRegistered
              ? "You are Registered as a DRep"
              : dRep?.wasRegistered
              ? "Register Again as a dRep"
              : "Register as a DRep"
          }
        />
      </Box>
      {!dRep?.isRegistered && renderGovActionSection()}
    </Box>
  );
};
