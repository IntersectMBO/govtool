import { useMemo } from "react";
import { Box, Link } from "@mui/material";

import { Button, Input, LoadingButton, Typography } from "../atoms";
import { useScreenDimension, useDelegateTodRepForm } from "@hooks";
import { theme } from "@/theme";
import { openInNewTab } from "@utils";

interface DelegateProps {
  setStep: (newStep: number) => void;
}

export const DelegateTodRepStepTwo = ({ setStep }: DelegateProps) => {
  const { isMobile } = useScreenDimension();

  const {
    palette: { boxShadow2 },
  } = theme;

  const { control, delegate, isDelegateButtonDisabled, isDelegationLoading } =
    useDelegateTodRepForm();

  const renderDelegateButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid={"delegate-button"}
        disabled={isDelegateButtonDisabled}
        isLoading={isDelegationLoading}
        onClick={delegate}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="contained"
      >
        Delegate
      </LoadingButton>
    );
  }, [isDelegateButtonDisabled, delegate, isMobile, isDelegationLoading]);

  const renderBackButton = useMemo(() => {
    return (
      <Button
        data-testid={"cancel-button"}
        onClick={() => setStep(1)}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="outlined"
      >
        Back
      </Button>
    );
  }, [isMobile]);

  return (
    <Box
      boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
      px={isMobile ? 2 : 17.5}
      py={isMobile ? 4 : 14}
      borderRadius={"20px"}
      mb={isMobile ? 0 : 6}
      flex={1}
      maxWidth={646}
      display="flex"
      flexDirection="column"
    >
      <Box display="flex" flexDirection="column" flex={1} px={isMobile ? 0 : 6}>
        <Typography sx={{ textAlign: "center" }} variant="headline4">
          Paste DRep ID
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ mb: 6, mt: 1, textAlign: "center" }}
          variant="body1"
        >
          The DRep ID is the identifier of a DRep.
        </Typography>
        <Box display="flex" justifyContent="center">
          <Input
            control={control}
            formFieldName="dRepID"
            placeholder="Paste DRep ID"
            dataTestId="dRep-id-input"
            width={"100%"}
          />
        </Box>
        <Link
          data-testid={"find-dRep-link"}
          onClick={() =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/where-can-i-find-a-drep-id"
            )
          }
          alignSelf={"center"}
          mt={4}
          sx={[{ "&:hover": { cursor: "pointer" } }]}
        >
          <Typography color="primary" fontWeight={500} variant="body2">
            Where can I find a DRep ID?
          </Typography>
        </Link>
      </Box>
      <Box
        display={"flex"}
        flexDirection={isMobile ? "column" : "row"}
        justifyContent={"space-between"}
        mt={6}
        px={isMobile ? 0 : 6}
      >
        {isMobile ? renderDelegateButton : renderBackButton}
        <Box px={2} py={isMobile ? 1.5 : 0} />
        {isMobile ? renderBackButton : renderDelegateButton}
      </Box>
    </Box>
  );
};
