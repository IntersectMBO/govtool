import { useMemo } from "react";
import { Box, Link } from "@mui/material";

import { Button, Input, Typography } from "../atoms";
import {
  useScreenDimension,
  useDelegateTodRepForm,
  useTranslation,
} from "@hooks";
import { theme } from "@/theme";
import { openInNewTab } from "@utils";

interface DelegateProps {
  setStep: (newStep: number) => void;
}

export const DelegateTodRepStepTwo = ({ setStep }: DelegateProps) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const {
    palette: { boxShadow2 },
  } = theme;

  const { control, isDelegateButtonDisabled, delegate } =
    useDelegateTodRepForm();

  const renderDelegateButton = useMemo(() => {
    return (
      <Button
        data-testid={"delegate-button"}
        disabled={isDelegateButtonDisabled}
        onClick={delegate}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="contained"
      >
        {t("delegate")}
      </Button>
    );
  }, [isDelegateButtonDisabled, delegate, isMobile]);

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
        {t("back")}
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
          {t("delegation.pasteDRepId")}
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ mb: 6, mt: 1, textAlign: "center" }}
          variant="body1"
        >
          {t("delegation.dRepIdDescription")}
        </Typography>
        <Box display="flex" justifyContent="center">
          <Input
            control={control}
            formFieldName="dRepId"
            placeholder={t("delegation.pasteDRepId")}
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
            {t("delegation.whereFindDRepId")}
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
