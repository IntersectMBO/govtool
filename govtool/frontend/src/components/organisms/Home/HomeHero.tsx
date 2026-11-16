import { Box } from "@mui/material";
import { useNavigate } from "react-router";
import { ArrowUpRight, Columns3, Rows3, UsersRound } from "lucide-react";

import { Button } from "@atoms";
import { PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { ds } from "@/consts/designTokens";
import { LINKS } from "@/consts/links";

import { GovernanceSnapshot } from "./GovernanceSnapshot";
import { DisplayHeading, Eyebrow, HomeColumn, Lead } from "./homeParts";

const PARTICIPATION = [
  { key: "delegate", href: LINKS.DELEGATING, Icon: Rows3, dataTestId: "delegate-learn-more-button" },
  { key: "represent", href: LINKS.REGISTER_AS_A_DREP, Icon: UsersRound, dataTestId: "d-rep-learn-more-button" },
  { key: "direct", href: LINKS.DIRECT_VOTING, Icon: Columns3, dataTestId: "direct-voter-learn-more-button" },
] as const;

const Participation = () => {
  const { t } = useTranslation();

  return (
    <Box sx={{ borderTop: `1px solid ${ds.gray["300"]}`, pb: { xxs: 6, md: 10 }, pt: { xxs: 5, md: 7.5 } }}>
      <Box sx={{ alignItems: { md: "flex-end" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ maxWidth: 400 }}>
          <Eyebrow color={ds.gray["400"]}>{t("landing.participation.eyebrow")}</Eyebrow>
          <DisplayHeading size="sub" sx={{ mt: 2 }}>
            {t("landing.participation.title")}
          </DisplayHeading>
        </Box>
        <Lead sx={{ textAlign: { md: "right" } }}>{t("landing.participation.aside")}</Lead>
      </Box>
      <Box
        sx={{
          background: "linear-gradient(180deg, #FFFFFF 0%, rgba(255, 255, 255, 0) 100%)",
          border: `1px solid ${ds.gray["200"]}`,
          display: "grid",
          gridTemplateColumns: { xxs: "1fr", md: "repeat(3, 1fr)" },
          mt: 4,
        }}
      >
        {PARTICIPATION.map(({ key, href, Icon, dataTestId }, index) => (
          <Box
            key={key}
            component="a"
            data-testid={dataTestId}
            href={href}
            rel="noopener noreferrer"
            target="_blank"
            sx={{
              borderLeft: { md: index ? `1px solid ${ds.gray["200"]}` : "none" },
              borderTop: { xxs: index ? `1px solid ${ds.gray["200"]}` : "none", md: "none" },
              color: "inherit",
              display: "flex",
              flexDirection: "column",
              gap: 1,
              p: 4,
              textDecoration: "none",
              transition: "background-color 0.2s",
              "&:hover": { backgroundColor: ds.gray["50"] },
            }}
          >
            <Box sx={{ alignItems: "flex-start", display: "flex", justifyContent: "space-between", mb: 2 }}>
              <Box sx={{ alignItems: "center", backgroundColor: ds.gray["100"], borderRadius: "50%", color: ds.gray["600"], display: "flex", height: 32, justifyContent: "center", width: 32 }}>
                <Icon aria-hidden size={16} />
              </Box>
              <Box component="span" sx={{ color: ds.gray["500"], fontSize: 9, fontWeight: 600, letterSpacing: "1.4px", lineHeight: "12px", textTransform: "uppercase" }}>
                {t(`landing.participation.${key}.step`)}
              </Box>
            </Box>
            <DisplayHeading component="h3" size="card">
              {t(`landing.participation.${key}.title`)}
            </DisplayHeading>
            <Lead>{t(`landing.participation.${key}.description`)}</Lead>
          </Box>
        ))}
      </Box>
    </Box>
  );
};

/** The landing hero, its live snapshot card and the three ways to take part. */
export const HomeHero = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();

  return (
    <Box
      component="section"
      sx={{
        backgroundImage: `radial-gradient(${ds.gray["200"]} 1px, transparent 1px)`,
        backgroundSize: "16px 16px",
        position: "relative",
      }}
    >
      <HomeColumn>
        <Box
          sx={{
            alignItems: "center",
            display: "flex",
            flexDirection: { xxs: "column", lg: "row" },
            gap: { xxs: 5, lg: 9 },
            py: { xxs: 5, md: 7.5 },
          }}
        >
          <Box sx={{ flex: 1, maxWidth: 546, width: "100%" }}>
            <Box sx={{ alignItems: "center", display: "flex", gap: 1.25 }}>
              <Box sx={{ backgroundColor: ds.success["600"], borderRadius: "50%", height: 8, width: 8 }} />
              <Eyebrow color={ds.success["700"]}>{t("landing.eyebrow")}</Eyebrow>
            </Box>
            <DisplayHeading component="h1" size="hero" sx={{ mt: 2 }}>
              {t("landing.titleLine1")}
              <br />
              {t("landing.titleLine2")}
              <Box component="span" sx={{ color: ds.primary["600"] }}>
                {t("landing.titleHighlight")}
              </Box>
            </DisplayHeading>
            <Lead size={16} sx={{ maxWidth: 420, mt: 3 }}>
              {t("landing.description")}
            </Lead>
            <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5, mt: 3 }}>
              <Button
                data-testid="move-to-governance-actions-button"
                endIcon={<ArrowUpRight aria-hidden size={16} />}
                onClick={() => navigate(PATHS.governanceActions)}
              >
                {t("landing.viewGovernanceActions")}
              </Button>
              <Button
                data-testid="view-drep-directory-button"
                onClick={() => navigate(PATHS.dRepDirectory)}
                variant="outlined"
                sx={{ color: ds.gray["900"] }}
              >
                {t("landing.findDRep")}
              </Button>
            </Box>
            <Box component="p" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px", m: 0, mt: 3 }}>
              {t("landing.noWalletNeeded")}
            </Box>
          </Box>
          <GovernanceSnapshot />
        </Box>
        <Participation />
      </HomeColumn>
    </Box>
  );
};
