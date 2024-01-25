import { Box, Button, Typography } from "@mui/material";

import { ModalContents, ModalHeader, ModalWrapper } from "@atoms";
import { IMAGES } from "@consts";
import { useModal } from "@context";
import { useScreenDimension } from "@hooks";
import { theme } from "@/theme";
import { openInNewTab } from "@utils";
import { usei18n } from "@translations";

export interface ExternalLinkModalState {
  externalLink: string;
}

export function ExternalLinkModal() {
  const { state, closeModal } = useModal<ExternalLinkModalState>();
  const { isMobile } = useScreenDimension();
  const { t } = usei18n();
  const {
    palette: { primaryBlue, fadedPurple },
  } = theme;

  return (
    <ModalWrapper dataTestId="external-link-modal">
      <img
        alt="Status icon"
        src={IMAGES.warningYellowImage}
        style={{ height: "84px", margin: "0 auto", width: "84px" }}
      />
      <ModalHeader sx={{ marginTop: "34px" }}>
        {isMobile
          ? t("modals.externalLink.safety")
          : t("modals.externalLink.beCareful")}
      </ModalHeader>
      <ModalContents>
        <Typography textAlign="center" sx={{ fontSize: "16px" }}>
          {isMobile
            ? t("modals.externalLink.thisIs")
            : t("modals.externalLink.youAreAboutToOpen")}
        </Typography>
        <Typography
          textAlign="center"
          sx={{
            fontSize: "16px",
            marginBottom: "38px",
            color: primaryBlue,
            textDecoration: "underline",
          }}
        >
          {state?.externalLink}
        </Typography>
        <Typography
          textAlign="center"
          sx={{
            fontSize: isMobile ? "16px" : "14px",
            marginBottom: "38px",
            color: fadedPurple,
          }}
        >
          {t("modals.externalLink.description")}
        </Typography>
      </ModalContents>
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          justifyContent: "center",
          gap: "38px",
        }}
      >
        <Button
          data-testid={"continue-modal-button"}
          onClick={() => {
            openInNewTab(state?.externalLink || "#");
            closeModal();
          }}
          sx={{
            borderRadius: 50,
            textTransform: "none",
            height: "40px",
          }}
          variant="contained"
        >
          {isMobile ? t("continue") : t("modals.externalLink.continueTo")}
        </Button>
        <Button
          data-testid={"cancel-modal-button"}
          onClick={() => {
            closeModal();
          }}
          sx={{
            borderRadius: 50,
            padding: "10px 26px",
            textTransform: "none",
            height: "40px",
            width: "117px",
          }}
          variant="outlined"
        >
          {t("cancel")}
        </Button>
      </Box>
    </ModalWrapper>
  );
}
