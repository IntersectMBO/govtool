import { Box, ButtonBase } from "@mui/material";

import { IMAGES } from "@consts";
import { useChatwoot } from "@context";
import { useTranslation } from "@hooks";
import { testIdFromLabel } from "@utils";
import { LINKS } from "@/consts/links";
import { ds } from "@/consts/designTokens";

const linkSx = {
  background: "none",
  border: "none",
  color: ds.gray["500"],
  cursor: "pointer",
  fontFamily: "inherit",
  fontSize: 12,
  fontWeight: 400,
  letterSpacing: "0.01px",
  lineHeight: "18px",
  p: 0,
  textDecoration: "none",
  "&:hover": { color: ds.gray["900"] },
  "&:focus-visible, &.Mui-focusVisible": {
    borderRadius: "2px",
    outline: `2px solid ${ds.primary["600"]}`,
    outlineOffset: 2,
  },
};

type FooterLinkProps = {
  href: string;
  label: string;
  dataTestId?: string;
};

const FooterLink = ({ href, label, dataTestId }: FooterLinkProps) => (
  <Box
    component="a"
    data-testid={dataTestId ?? `${testIdFromLabel(label)}-footer-link`}
    href={href}
    rel="noopener noreferrer"
    sx={linkSx}
    target="_blank"
  >
    {label}
  </Box>
);

const rowSx = {
  alignItems: { xxs: "flex-start", md: "center" },
  display: "flex",
  flexDirection: { xxs: "column", md: "row" },
  gap: 2,
  justifyContent: "space-between",
};

const linkGroupSx = {
  display: "flex",
  flexWrap: "wrap",
  gap: 3,
};

export const Footer = () => {
  const { t } = useTranslation();
  const { openFeedbackWindow } = useChatwoot();

  return (
    <Box
      component="footer"
      sx={{
        backgroundColor: "#FCFCFA",
        pt: 7,
        px: { xxs: 2, lg: 3 },
      }}
    >
      <Box sx={{ maxWidth: 1128, mx: "auto" }}>
        <Box sx={{ ...rowSx, pb: 4.5 }}>
          <img
            alt="Cardano GovTool"
            height={32}
            src={IMAGES.govToolLogo}
            width={158}
          />
          <Box
            component="p"
            sx={{ ...linkSx, cursor: "default", m: 0, "&:hover": {} }}
          >
            {t("footer.tagline")}
          </Box>
          <Box sx={linkGroupSx}>
            <FooterLink href={LINKS.USING_GOVTOOL} label={t("menu.guides")} />
            <FooterLink
              dataTestId="faqs-link"
              href={LINKS.FAQS}
              label={t("menu.faqs")}
            />
            <FooterLink
              dataTestId="intersect-footer-link"
              href={LINKS.INTERSECT}
              label={t("footer.intersect")}
            />
          </Box>
        </Box>
        <Box
          sx={{
            ...rowSx,
            borderTop: `1px solid ${ds.gray["200"]}`,
            pb: "30px",
            pt: "23px",
          }}
        >
          <Box component="p" sx={{ ...linkSx, cursor: "default", m: 0 }}>
            {t("footer.copyright", { year: new Date().getFullYear() })}
          </Box>
          <Box sx={linkGroupSx}>
            <FooterLink
              href={LINKS.PRIVACY_POLICY}
              label={t("footer.privacyPolicy")}
            />
            <FooterLink
              href={LINKS.TERMS_OF_USE}
              label={t("footer.termOfService")}
            />
            <FooterLink
              dataTestId="help-footer-button"
              href={LINKS.SUPPORT}
              label={t("menu.help")}
            />
            <FooterLink
              dataTestId="newsletter-footer-button"
              href={LINKS.NEWSLETTER_SIGNUP}
              label={t("footer.subscribe")}
            />
            <ButtonBase
              data-testid="feedback-footer-button"
              disableRipple
              onClick={openFeedbackWindow}
              sx={linkSx}
            >
              {t("feedback")}
            </ButtonBase>
          </Box>
        </Box>
      </Box>
    </Box>
  );
};
