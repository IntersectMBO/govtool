"use client";
import { Box, Button, Link, Typography } from "@mui/material";
import HelpOutlineIcon from "@mui/icons-material/HelpOutline";

import { openInNewTab } from "@/utils";
import { useUsersnapApi } from "@/context/userSnapApi";

export const Footer = ({ sx }) => {
  const { openFeedbackWindow } = useUsersnapApi();

  const onClickHelp = () =>
    openInNewTab("https://docs.sanchogov.tools/support/get-help-in-discord");

  const onClickPrivacyPolicy = () =>
    openInNewTab("https://docs.sanchogov.tools/legal/privacy-policy");

  // TODO: change term of service link
  const onClickTermOfService = () =>
    openInNewTab("https://docs.sanchogov.tools/legal/privacy-policy");

  const onClickFeedback = () => openFeedbackWindow();

  return (
    <Box component="footer" sx={{ ...style.container, ...sx }}>
      <Typography variant="bodyMediumS">© 2024 Intersect MBO</Typography>
      <Box sx={style.linksContainer}>
        <Link
          data-testid="policy-footer-link"
          onClick={onClickPrivacyPolicy}
          sx={style.link}
          variant="bodyRegularS"
        >
          Privacy policy
        </Link>
        <Link
          data-testid="terms-of-service-footer-link"
          onClick={onClickTermOfService}
          sx={style.link}
          variant="bodyRegularS"
        >
          Term of service
        </Link>
      </Box>
      <Box sx={style.buttonsContainer}>
        <Button
          data-testid="help-footer-button"
          onClick={onClickHelp}
          size="small"
          startIcon={<HelpOutlineIcon color="orange" />}
          sx={style.helpButton}
          variant="link"
        >
          Help
        </Button>
        <Button
          data-testid="feedback-footer-button"
          onClick={onClickFeedback}
          size="small"
          variant="outlined"
        >
          Feedback
        </Button>
      </Box>
    </Box>
  );
};

const style = {
  buttonsContainer: {
    alignItems: "center",
    display: "flex",
    flexDirection: "row",
    gap: 3,
    justifyContent: { xxs: "space-between", md: undefined },
    mt: { xxs: 1.5, md: 0 },
  },
  container: {
    alignItems: { xxs: undefined, md: "center" },
    display: "flex",
    flexDirection: { xxs: "column", md: "row" },
    justifyContent: "space-between",
    paddingX: { xxs: 2, md: 5 },
    paddingY: 2,
  },
  helpButton: { color: "fadedPurple.800" },
  link: {
    color: "black",
    cursor: "pointer",
    textDecoration: "none",
    "&:hover": { color: "primary.500" },
  },
  linksContainer: {
    display: "flex",
    flexDirection: "row",
    gap: 3,
    mt: { xxs: 1.5, md: 0 },
  },
};
