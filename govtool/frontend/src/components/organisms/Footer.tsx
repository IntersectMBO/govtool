import { Box, Link } from "@mui/material";

import { Button, Typography } from "@atoms";
import { ICONS } from "@consts";
import { useUsersnapApi } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab, testIdFromLabel } from "@utils";

type FooterLinkProps = {
  label: string;
  onClick: () => void;
};

const FooterLink = ({ label, onClick }: FooterLinkProps) => (
  <Link
    data-testid={`${testIdFromLabel(label)}-footer-link`}
    onClick={onClick}
    sx={{
      color: "textBlack",
      cursor: "pointer",
      fontFamily: "Poppins",
      fontSize: 12,
      fontWeight: 400,
      textDecoration: "none",
      "&:hover": { color: "primaryBlue" },
    }}
  >
    {label}
  </Link>
);

export const Footer = () => {
  const { screenWidth } = useScreenDimension();
  const { t } = useTranslation();
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
    <>
      <Box
        component="footer"
        sx={{
          alignItems: screenWidth < 640 ? undefined : "center",
          display: "flex",
          flexDirection: screenWidth < 640 ? "column" : "row",
          justifyContent: "space-between",
          px: screenWidth < 640 ? 2 : 5,
          py: 2,
        }}
      >
        <Typography fontWeight={500} variant="caption">
          {t("footer.copyright")}
        </Typography>
        <Box
          sx={{
            display: "flex",
            flexDirection: "row",
            gap: 3,
            mt: screenWidth < 640 ? 1.5 : 0,
          }}
        >
          <FooterLink
            label={t("footer.privacyPolicy")}
            onClick={onClickPrivacyPolicy}
          />
          <FooterLink
            label={t("footer.termOfService")}
            onClick={onClickTermOfService}
          />
        </Box>
        <Box
          sx={{
            display: "flex",
            flexDirection: "row",
            gap: 3,
            justifyContent: screenWidth < 640 ? "space-between" : undefined,
            mt: screenWidth < 640 ? 1.5 : 0,
          }}
        >
          <Button
            data-testid="help-footer-button"
            onClick={onClickHelp}
            size="small"
            startIcon={<img alt="helpIcon" src={ICONS.helpIcon} />}
            sx={{ color: "#26252D" }}
            variant="text"
          >
            {t("menu.help")}
          </Button>
          <Button
            data-testid="feedback-footer-button"
            onClick={onClickFeedback}
            size="small"
            variant="outlined"
          >
            {t("feedback")}
          </Button>
        </Box>
      </Box>
    </>
  );
};
