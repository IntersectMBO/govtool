import { Box, Link } from "@mui/material";

import { useTranslation } from "@hooks";
import { ICONS } from "@/consts";

import { Typography } from "../../atoms";
import { Card } from "../../molecules";

const LINKS = {
  ccPortal: {
    url: "https://constitution.gov.tools",
  },
  intersectWebsite: {
    url: "https://www.intersectmbo.org/",
  },
  tempo: {
    url: "https://tempo.vote/",
  },
  "1694io": {
    url: "https://www.1694.io/",
  },
  governanceSpace: {
    url: "https://governancespace.com/",
  },
  syncAi: {
    url: "https://www.syncgovhub.com/app",
  },
  ekklesia: {
    url: "https://2025budget.intersectmbo.org/",
  },
  adaStat: {
    url: "https://adastat.net/",
  },
  cexplorer: {
    url: "https://cexplorer.io/",
  },
  cardanoScan: {
    url: "https://cardanoscan.io/",
  },
  cardanoBudget: {
    url: "https://cardanobudget.com/",
  },
  budgetCardanoAfrica: {
    url: "https://budget.cardano.africa/",
  },
  reachYourPeople: {
    url: "https://www.ryp.io/",
  },
  "1694Tools": {
    url: "https://1694-tools.vercel.app/",
  },
  sanchonetGovernanceExplorer: {
    url: "https://sancho.cardanoconnect.io/",
  },
} as const;

type Props = {
  align?: "left" | "center";
};

export const UsefulLinks = ({ align = "left" }: Props) => {
  const { t } = useTranslation();

  return (
    <div>
      <Typography variant="title2" sx={{ mb: 4, textAlign: align }}>
        {t("usefulLinks.title")}
      </Typography>
      <Box
        display="grid"
        gridTemplateColumns={{
          xxs: "repeat(1, 1fr)",
          sm: "repeat(2, 1fr)",
          lg: "repeat(4, 1fr)",
        }}
        gap={4}
        mt={4}
      >
        {Object.entries(LINKS).map(([key, { url }]) => (
          <Card
            key={key}
            sx={{
              flexBasis: 0,
              boxShadow: "2px 2px 20px 0px rgba(47, 98, 220, 0.20)",
              minHeight: 196,
              boxSizing: "border-box",
              display: "flex",
              flexDirection: "column",
              gap: 1,
            }}
          >
            <Typography>
              {t(`usefulLinks.${key as keyof typeof LINKS}.title`)}
            </Typography>
            <Typography variant="caption" sx={{ mb: 1 }}>
              {t(`usefulLinks.${key as keyof typeof LINKS}.description`)}
            </Typography>
            <Link
              data-testid={`useful-link-${key}`}
              href={url}
              target="_blank"
              sx={{
                alignSelf: "flex-start",
                display: "flex",
                gap: 1,
                alignItems: "center",
                mt: "auto",
                "&:not(:hover)": {
                  textDecoration: "none",
                },
              }}
            >
              <Typography color="primary" variant="body2">
                {t(`usefulLinks.${key as keyof typeof LINKS}.link`)}
              </Typography>
              <img
                alt="link"
                height={16}
                src={ICONS.externalLinkIcon}
                width={16}
              />
            </Link>
          </Card>
        ))}
      </Box>
    </div>
  );
};
