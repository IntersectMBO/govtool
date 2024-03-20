import {
  useEffect, useState, useCallback, useMemo,
} from "react";
import { useNavigate } from "react-router-dom";
import { Box, Grid } from "@mui/material";

import {
  ActionRadio, Button, LoadingButton, Typography,
} from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import {
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { correctAdaFormat } from "@utils";
import { theme } from "@/theme";

interface DelegateProps {
  setStep: (newStep: number) => void;
}

export const DelegateTodRepStepOne = ({ setStep }: DelegateProps) => {
  const navigate = useNavigate();
  const {
    voter,
    dRepID,
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
    stakeKey,
  } = useCardano();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { openModal, closeModal } = useModal();
  const [areOptions, setAreOptions] = useState<boolean>(false);
  const [chosenOption, setChosenOption] = useState<string>("");
  const [isDelegationLoading, setIsDelegationLoading] = useState<boolean>(false);
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const correctAdaRepresentation = correctAdaFormat(votingPower);

  const openSuccessDelegationModal = useCallback(() => {
    openModal({
      type: "statusModal",
      state: {
        status: "success",
        title: t("modals.delegation.title"),
        message: t("modals.delegation.message"),
        link: "https://adanordic.com/latest_transactions",
        buttonText: t("modals.common.goToDashboard"),
        onSubmit: () => {
          navigate(PATHS.dashboard);
          closeModal();
        },
        dataTestId: "delegation-transaction-submitted-modal",
      },
    });
  }, []);

  const openErrorDelegationModal = useCallback((errorMessage: string) => {
    openModal({
      type: "statusModal",
      state: {
        status: "warning",
        title: t("modals.delegation.title"),
        message: errorMessage,
        isWarning: true,
        buttonText: t("modals.common.goToDashboard"),
        onSubmit: () => {
          navigate(PATHS.dashboard);
          closeModal();
        },
        dataTestId: "delegation-transaction-error-modal",
      },
    });
  }, []);

  useEffect(() => {
    if (
      !areOptions
      && (chosenOption === "no confidence" || chosenOption === "abstain")
    ) {
      setChosenOption("");
    }
  }, [chosenOption, areOptions]);

  const delegate = useCallback(async () => {
    setIsDelegationLoading(true);
    try {
      const certBuilder = await buildVoteDelegationCert(chosenOption);
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "delegation",
      });
      if (result) openSuccessDelegationModal();
    } catch (error: any) {
      const errorMessage = error.info ? error.info : error;

      openErrorDelegationModal(errorMessage);
    } finally {
      setIsDelegationLoading(false);
    }
  }, [chosenOption, buildSignSubmitConwayCertTx, buildVoteDelegationCert]);

  const renderDelegateButton = useMemo(() => (
    <LoadingButton
      data-testid={
          chosenOption !== dRepID ? "next-step-button" : "delegate-button"
        }
      disabled={!chosenOption}
      isLoading={isDelegationLoading}
      onClick={() => {
        if (chosenOption === "Delegate to DRep") {
          setStep(2);
        } else {
          delegate();
        }
      }}
      size="extraLarge"
      sx={{
        px: 6,
        width: isMobile ? "100%" : "auto",
      }}
      variant="contained"
    >
      {chosenOption !== dRepID ? t("nextStep") : t("delegate")}
    </LoadingButton>
  ), [
    chosenOption,
    delegate,
    voter?.isRegisteredAsDRep,
    voter?.isRegisteredAsSoleVoter,
    dRepID,
    isDelegationLoading,
    isMobile,
  ]);

  const renderCancelButton = useMemo(() => (
    <Button
      data-testid="cancel-button"
      onClick={() => navigate(PATHS.dashboard)}
      size="extraLarge"
      sx={{
        px: 6,
        width: isMobile ? "100%" : "auto",
      }}
      variant="outlined"
    >
      {t("cancel")}
    </Button>
  ), [isMobile]);

  return (
    <Box
      boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
      px={isMobile ? 2 : 17.5}
      pb={isMobile ? 4 : 5}
      pt={isMobile ? 4 : 8.5}
      borderRadius="20px"
      mb={isMobile ? 0 : 6}
      height="100%"
    >
      <Box>
        {!isMobile && (
          <Box
            sx={{
              display: "flex",
              justifyContent: "center",
            }}
          >
            <Box
              border={1}
              borderColor="#D6E2FF"
              py="12px"
              px="24px"
              mb="32px"
              borderRadius={3}
            >
              <Box sx={{ display: "flex", alignItems: "center" }}>
                <Typography color="textBlack" variant="body2">
                  {t("delegation.votingPowerToDelegate")}
                </Typography>
                <Typography
                  color="specialCyan"
                  fontSize={18}
                  fontWeight={600}
                  sx={{ marginLeft: "10px", paddingBottom: "1px" }}
                >
                  {`₳ ${correctAdaRepresentation}`}
                </Typography>
              </Box>
            </Box>
          </Box>
        )}
        <Typography sx={{ textAlign: "center" }} variant="headline4">
          {t("delegation.heading")}
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ mb: 4, mt: 1, textAlign: "center" }}
          variant="body1"
        >
          {t("delegation.description")}
        </Typography>
        <Grid
          container
          columns={1}
          display="flex"
          flexDirection="column"
          rowGap={3}
        >
          {(voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter)
            && currentDelegation !== dRepID && (
              <Grid item>
                <ActionRadio
                  onChange={setChosenOption}
                  tooltipTitle={t("tooltips.delegateTodRep.toMyself.heading")}
                  tooltipText={t(
                    "tooltips.delegateTodRep.toMyself.paragraphOne",
                  )}
                  isChecked={chosenOption === dRepID}
                  subtitle={t("delegation.toMyself.subtitle")}
                  title={t("delegation.toMyself.title")}
                  value={dRepID}
                  dataTestId="delegate-to-myself-card"
                />
              </Grid>
          )}
          <Grid item>
            <ActionRadio
              onChange={setChosenOption}
              tooltipTitle={t("tooltips.delegateTodRep.todRep.heading")}
              tooltipText={t("tooltips.delegateTodRep.todRep.paragraphOne")}
              isChecked={chosenOption === "Delegate to DRep"}
              subtitle={t("delegation.toDRep.subtitle")}
              title={t("delegation.toDRep.title")}
              value="Delegate to DRep"
              dataTestId="delegate-to-drep-card"
            />
          </Grid>
          <Box
            data-testid="other-options"
            onClick={() => setAreOptions((prev) => !prev)}
            textAlign="center"
            sx={[
              {
                "&:hover": { cursor: "pointer" },
                display: "flex",
                alignItems: "center",
                justifyContent: "center",
              },
            ]}
          >
            <Typography variant="caption">
              {t("delegation.otherOptions")}
            </Typography>
            <img
              alt="arrow"
              src={ICONS.arrowDownIcon}
              style={{
                marginLeft: "20px",
                transform: `rotate(${areOptions ? "180deg" : "0"})`,
              }}
            />
          </Box>
          {areOptions ? (
            <>
              <Grid item>
                <ActionRadio
                  tooltipTitle={t(
                    "tooltips.delegateTodRep.noConfidence.heading",
                  )}
                  tooltipText={t(
                    "tooltips.delegateTodRep.noConfidence.paragraphOne",
                  )}
                  onChange={setChosenOption}
                  isChecked={chosenOption === "no confidence"}
                  subtitle={t("delegation.noConfidence.subtitle")}
                  title={t("delegation.noConfidence.title")}
                  value="no confidence"
                  dataTestId="signal-no-confidence-card"
                />
              </Grid>
              <Grid item>
                <ActionRadio
                  onChange={setChosenOption}
                  tooltipTitle={t("tooltips.delegateTodRep.abstain.heading")}
                  tooltipText={t(
                    "tooltips.delegateTodRep.abstain.paragraphOne",
                  )}
                  isChecked={chosenOption === "abstain"}
                  subtitle={t("delegation.abstain.subtitle")}
                  title={t("delegation.abstain.title")}
                  value="abstain"
                  dataTestId="vote-abstain-card"
                />
              </Grid>
            </>
          ) : null}
        </Grid>
      </Box>
      <Box
        display="flex"
        flexDirection={isMobile ? "column" : "row"}
        justifyContent="space-between"
        mt={6}
      >
        {isMobile ? renderDelegateButton : renderCancelButton}
        <Box px={2} py={isMobile ? 1.5 : 0} />
        {isMobile ? renderCancelButton : renderDelegateButton}
      </Box>
    </Box>
  );
};
