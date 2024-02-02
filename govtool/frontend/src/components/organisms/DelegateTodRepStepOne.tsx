import { useEffect, useState, useCallback, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Grid } from "@mui/material";

import { ActionRadio, Button, LoadingButton, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import {
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
} from "@hooks";
import { theme } from "@/theme";
import { tooltips } from "@/consts/texts";
import { correctAdaFormat } from "@utils";

interface DelegateProps {
  setStep: (newStep: number) => void;
}

export const DelegateTodRepStepOne = ({ setStep }: DelegateProps) => {
  const navigate = useNavigate();
  const {
    dRep,
    dRepID,
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
    stakeKey,
  } = useCardano();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { openModal, closeModal } = useModal();
  const [areOptions, setAreOptions] = useState<boolean>(false);
  const [chosenOption, setChosenOption] = useState<string>("");
  const [isDelegationLoading, setIsDelegationLoading] =
    useState<boolean>(false);
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile } = useScreenDimension();

  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const correctAdaRepresentation = correctAdaFormat(votingPower);

  const openSuccessDelegationModal = useCallback(() => {
    openModal({
      type: "statusModal",
      state: {
        status: "success",
        title: "Delegation Transaction Submitted!",
        message:
          "The confirmation of your actual delegation might take a bit of time but you can track it using.",
        link: "https://adanordic.com/latest_transactions",
        buttonText: "Go to dashboard",
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
        title: "Oops!",
        message: errorMessage,
        isWarning: true,
        buttonText: "Go to dashboard",
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
      !areOptions &&
      (chosenOption === "no confidence" || chosenOption === "abstain")
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

  const renderDelegateButton = useMemo(() => {
    return (
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
        {chosenOption !== dRepID ? "Next step" : "Delegate"}
      </LoadingButton>
    );
  }, [
    chosenOption,
    delegate,
    dRep?.isRegistered,
    dRepID,
    isDelegationLoading,
    isMobile,
  ]);

  const renderCancelButton = useMemo(() => {
    return (
      <Button
        data-testid={"cancel-button"}
        onClick={() => navigate(PATHS.dashboard)}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="outlined"
      >
        Cancel
      </Button>
    );
  }, [isMobile]);

  return (
    <Box
      boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
      px={isMobile ? 2 : 17.5}
      pb={isMobile ? 4 : 5}
      pt={isMobile ? 4 : 8.5}
      borderRadius={"20px"}
      mb={isMobile ? 0 : 6}
      height={"100%"}
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
              py={"12px"}
              px={"24px"}
              mb={"32px"}
              borderRadius={3}
            >
              <Box sx={{ display: "flex", alignItems: "center" }}>
                <Typography color="textBlack" variant="body2">
                  Voting power to delegate:
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
          Use your Voting Power
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ mb: 4, mt: 1, textAlign: "center" }}
          variant="body1"
        >
          You can delegate your voting power to a DRep or to a pre-defined
          voting option.
        </Typography>
        <Grid
          container
          columns={1}
          display={"flex"}
          flexDirection={"column"}
          rowGap={3}
        >
          {dRep?.isRegistered && currentDelegation !== dRepID && (
            <Grid item>
              <ActionRadio
                onChange={setChosenOption}
                tooltipTitle={tooltips.delegateTodRep.toMyself.heading}
                tooltipText={tooltips.delegateTodRep.toMyself.paragraphOne}
                isChecked={chosenOption === dRepID}
                subtitle="Select this to delegate your own voting power to yourself."
                title="Delegate to myself"
                value={dRepID}
                dataTestId="delegate-to-myself-card"
              />
            </Grid>
          )}
          <Grid item>
            <ActionRadio
              onChange={setChosenOption}
              tooltipTitle={tooltips.delegateTodRep.todRep.heading}
              tooltipText={tooltips.delegateTodRep.todRep.paragraphOne}
              isChecked={chosenOption === "Delegate to DRep"}
              subtitle="Select this to delegate to a DRep using their related DRep ID."
              title="Delegate to DRep"
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
            <Typography variant="caption">Other options</Typography>
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
                  tooltipTitle={tooltips.delegateTodRep.noConfidence.heading}
                  tooltipText={
                    tooltips.delegateTodRep.noConfidence.paragraphOne
                  }
                  onChange={setChosenOption}
                  isChecked={chosenOption === "no confidence"}
                  subtitle="Select this to signal no confidence in the current constitutional committee by voting NO on every proposal and voting YES to no-confidence proposals."
                  title="Signal no confidence"
                  value="no confidence"
                  dataTestId="signal-no-confidence-card"
                />
              </Grid>
              <Grid item>
                <ActionRadio
                  onChange={setChosenOption}
                  tooltipTitle={tooltips.delegateTodRep.abstain.heading}
                  tooltipText={tooltips.delegateTodRep.abstain.paragraphOne}
                  isChecked={chosenOption === "abstain"}
                  subtitle="Select this to vote ABSTAIN to every vote."
                  title="Vote ABSTAIN as default"
                  value="abstain"
                  dataTestId="vote-abstain-card"
                />
              </Grid>
            </>
          ) : null}
        </Grid>
      </Box>
      <Box
        display={"flex"}
        flexDirection={isMobile ? "column" : "row"}
        justifyContent={"space-between"}
        mt={6}
      >
        {isMobile ? renderDelegateButton : renderCancelButton}
        <Box px={2} py={isMobile ? 1.5 : 0} />
        {isMobile ? renderCancelButton : renderDelegateButton}
      </Box>
    </Box>
  );
};
