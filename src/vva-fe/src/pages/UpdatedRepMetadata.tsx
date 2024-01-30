import { useMemo, useEffect } from "react";
import { Box, Link } from "@mui/material";
import { useNavigate } from "react-router-dom";

import { Background, Input, LoadingButton, Button, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useScreenDimension,
  useUpdatedRepMetadataForm,
  useTranslation,
} from "@hooks";
import { DashboardTopNav, Footer } from "@organisms";
import { theme } from "@/theme";
import { WALLET_LS_KEY, getItemFromLocalStorage, openInNewTab } from "@utils";

export const UpdatedRepMetadata = () => {
  const navigate = useNavigate();
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile, pagePadding, screenWidth } = useScreenDimension();
  const { isPendingTransaction } = useCardano();
  const { t } = useTranslation();

  const { submitForm, control, errors, isValid, isLoading } =
    useUpdatedRepMetadataForm();

  useEffect(() => {
    if (
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`) ||
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_name`)
    ) {
      navigate(PATHS.home);
    } else {
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) navigate(PATHS.home);
    }
  }, []);

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
        {t("cancel")}
      </Button>
    );
  }, [isMobile]);

  const renderUpdateButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid={"confirm-button"}
        disabled={!isValid}
        isLoading={isLoading}
        onClick={submitForm}
        sx={{
          borderRadius: 50,
          textTransform: "none",
          px: 6,
          width: isMobile ? "100%" : "auto",
          height: 48,
        }}
        variant="contained"
      >
        {t("confirm")}
      </LoadingButton>
    );
  }, [isLoading, isMobile, isValid, submitForm]);

  return (
    <Background isReverted>
      <Box display={"flex"} minHeight={"100vh"} flexDirection="column">
        <DashboardTopNav
          imageSRC={ICONS.appLogoIcon}
          imageWidth={isMobile ? undefined : 42}
          imageHeight={isMobile ? 24 : 35}
          title={t("metadataUpdate.title")}
        />
        <Box
          display={"flex"}
          justifyContent={"center"}
          mt={isMobile ? 0 : 7}
          height={isMobile ? "100%" : "auto"}
          sx={{ marginTop: isMobile ? "97px" : "153px" }}
        >
          <Box
            width={
              screenWidth < 768 ? "auto" : screenWidth < 1024 ? "60vw" : "45vw"
            }
            boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
            px={pagePadding}
            py={isMobile ? 4 : 8}
            borderRadius={3}
            mb={isMobile ? 0 : 6}
            height={"100%"}
          >
            <Box display="flex" flexDirection="column" alignItems="center">
              <Typography
                sx={{ mt: 1, textAlign: "center" }}
                variant="headline4"
              >
                {t("metadataUpdate.info")}
              </Typography>
              <Typography
                fontWeight={400}
                sx={{ mb: 7, mt: 3, textAlign: "cenetr" }}
                variant="body1"
              >
                {t("metadataUpdate.description")}
              </Typography>
              <Input
                control={control}
                formFieldName="url"
                placeholder={t("forms.urlWithInfoPlaceholder")}
                dataTestId="url-input"
                errorMessage={errors.url?.message}
                width={isMobile ? "100%" : "70%"}
              />
              <Input
                control={control}
                formFieldName="hash"
                placeholder={t("forms.hashPlaceholder")}
                dataTestId="hash-input"
                errorMessage={errors.hash?.message}
                width={isMobile ? "100%" : "70%"}
                marginTop="48px"
              />
              <Link
                onClick={() =>
                  openInNewTab(
                    "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor"
                  )
                }
                alignSelf={"center"}
                mt={5}
                sx={{ cursor: "pointer" }}
              >
                <Typography fontWeight={500} color="primary" variant="body1">
                  {t("forms.howCreateUrlAndHash")}
                </Typography>
              </Link>
            </Box>
            <Box
              display={"flex"}
              flexDirection={isMobile ? "column" : "row"}
              justifyContent={"space-between"}
              mt={6}
            >
              {isMobile ? renderUpdateButton : renderCancelButton}
              <Box px={2} py={isMobile ? 1.5 : 0} />
              {isMobile ? renderCancelButton : renderUpdateButton}
            </Box>
          </Box>
        </Box>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
