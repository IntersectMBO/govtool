import { Box, Button, Typography } from "@mui/material";

import { ModalContents, ModalHeader, ModalWrapper } from "@atoms";
import { IMAGES } from "@consts";
import { useModal } from "@context";
import { useScreenDimension } from "@hooks";
import { theme } from "@/theme";
import { openInNewTab } from "@utils";

export interface ExternalLinkModalState {
  externalLink: string;
}

export function ExternalLinkModal() {
  const { state, closeModal } = useModal<ExternalLinkModalState>();
  const { isMobile } = useScreenDimension();
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
        {isMobile ? "External Link Safety" : "Be Careful!"}
      </ModalHeader>
      <ModalContents>
        <Typography textAlign="center" sx={{ fontSize: "16px" }}>
          {isMobile
            ? "This is an external link:"
            : "You are about to open an external link to:"}
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
          Exercise caution and verify the website's authenticity before sharing
          personal information. To proceed, click 'Continue'. To stay on
          Voltaire, click 'Cancel'.
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
          {isMobile ? "Continue" : "Continue to external link"}
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
          Cancel
        </Button>
      </Box>
    </ModalWrapper>
  );
}
