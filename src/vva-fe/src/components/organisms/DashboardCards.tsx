import { useNavigate } from "react-router-dom";
import { Box, CircularProgress, Typography } from "@mui/material";
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
import { useCallback, useMemo, useState } from "react";
import { correctAdaFormat, formHexToBech32, openInNewTab } from "@utils";

export const DashboardCards = () => {
  const {
    dRepIDBech32: drepId,
    dRepID,
    dRep,
    stakeKey,
    buildSignSubmitConwayCertTx,
    delegateTransaction,
    buildDRepRetirementCert,
    registerTransaction,
    delegateTo,
    isPendingTransaction,
    isDrepLoading,
  } = useCardano();
  const navigate = useNavigate();
  const { currentDelegation, isCurrentDelegationLoading } =
    useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { screenWidth, isMobile } = useScreenDimension();
  const { openModal } = useModal();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const { votingPower, powerIsLoading } =
    useGetAdaHolderVotingPowerQuery(stakeKey);
  const { t } = useTranslation();

  const retireAsDrep = useCallback(async () => {
    try {
      setIsLoading(true);
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

      setIsLoading(false);
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
      setIsLoading(false);
    }
  }, [buildDRepRetirementCert, buildSignSubmitConwayCertTx]);

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
  }, [currentDelegation, drepId, votingPower]);

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
  }, [currentDelegation, drepId, votingPower]);

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
  }, [delegateTo, votingPower]);

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
          {t("dashboard.headingTwo")}
        </Typography>
        <Box display={"flex"}>
          <DashboardActionCard
            dataTestidFirstButton="view-governance-actions-button"
            description={t("dashboard.govActions.description")}
            firstButtonAction={() =>
              navigate(PATHS.dashboard_governance_actions)
            }
            firstButtonLabel={t(
              `dashboard.govActions.${
                dRep?.isRegistered ? "reviewAndVote" : "view"
              }`
            )}
            imageURL={IMAGES.govActionListImage}
            title={t("dashboard.govActions.title")}
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
        {t("dashboard.headingOne")}
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
              ? t("dashboard.delegation.changeDelegation")
              : t("delegate")
          }
          imageHeight={55}
          imageWidth={65}
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
        <Box width={24} />
        <DashboardActionCard
          dataTestidFirstButton={
            dRep?.isRegistered ? "retire-button" : "register-button"
          }
          dataTestidDrepIdBox="my-drep-id"
          firstButtonVariant={dRep?.isRegistered ? "outlined" : "contained"}
          firstButtonDisabled={isLoading}
          secondButtonVariant={
            registerTransaction?.transactionHash
              ? "outlined"
              : dRep?.isRegistered
              ? "text"
              : "outlined"
          }
          dataTestidSecondButton={
            dRep?.isRegistered && drepId
              ? "change-metadata-button"
              : "register-learn-more-button"
          }
          description={t(
            `dashboard.registration.${
              registerTransaction.transactionHash
                ? registerTransaction?.type === "retirement"
                  ? "retirementInProgress"
                  : registerTransaction?.type === "registration"
                  ? "registrationInProgress"
                  : "metadataUpdateInProgress"
                : dRep?.isRegistered
                ? "holdersCanDelegate"
                : dRep?.wasRegistered
                ? "holdersCanDelegate"
                : "ifYouWant"
            }`
          )}
          firstButtonAction={
            dRep?.isRegistered
              ? retireAsDrep
              : () => navigateTo(PATHS.registerAsdRep)
          }
          firstButtonLabel={
            registerTransaction?.transactionHash
              ? ""
              : t(
                  `dashboard.registration.${
                    dRep?.isRegistered ? "retire" : "register"
                  }`
                )
          }
          inProgress={!!registerTransaction?.transactionHash}
          imageURL={IMAGES.govActionRegisterImage}
          secondButtonAction={
            registerTransaction?.transactionHash
              ? () => openInNewTab("https://adanordic.com/latest_transactions")
              : dRep?.isRegistered && drepId
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
              : dRep?.isRegistered
              ? t("dashboard.registration.changeMetadata")
              : t("learnMore")
          }
          cardId={dRep?.isRegistered || dRep?.wasRegistered ? drepId : ""}
          cardTitle={
            dRep?.isRegistered || dRep?.wasRegistered ? t("myDRepId") : ""
          }
          title={t(
            `dashboard.registration.${
              registerTransaction?.transactionHash
                ? registerTransaction?.type === "retirement"
                  ? "dRepRetirement"
                  : registerTransaction?.type === "registration"
                  ? "dRepRegistration"
                  : "dRepUpdate"
                : dRep?.isRegistered
                ? "youAreRegistered"
                : dRep?.wasRegistered
                ? "registerAgain"
                : "registerAsDRep"
            }`
          )}
        />
      </Box>
      {!dRep?.isRegistered && renderGovActionSection()}
    </Box>
  );
};
