import { Box, Link } from "@mui/material";
import { Link as RouterLink } from "react-router";

import { useTranslation } from "@hooks";
import { Typography } from "../atoms";
import { BUDGET_DISCUSSION_PATHS, ICONS } from "@/consts";
import { Card } from "../molecules";

// External links open in a new tab; internal ones (the read-only 2025 budget
// proposals archive) stay in the app.
const LINKS = {
  intersectWebsite: {
    url: "https://www.intersectmbo.org/",
    external: true,
  },
  budgetProposalsArchive: {
    url: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
    external: false,
  },
} as const;

type Props = {
  align?: "left" | "center";
};

export const UsefulLinks = ({ align = "left" }: Props) => {
  const { t } = useTranslation();

  return (
    <div>
      <Typography variant="title1" sx={{ mb: 4, textAlign: align }}>
        {t("usefulLinks.title")}
      </Typography>
      <Box
        sx={{
          display: "flex",
          flexDirection: { xxs: "column", lg: "row" },
          columnGap: 4.5,
          rowGap: 2.5,
          justifyContent: align === "center" ? "center" : "flex-start",
          alignContent: "center",
          flexWrap: "wrap",
        }}
      >
        {Object.entries(LINKS).map(([key, { url, external }]) => (
          <Card
            key={key}
            sx={{
              flexBasis: 0,
              boxShadow: "2px 2px 20px 0px rgba(47, 98, 220, 0.20)",
              maxWidth: 464,
              minWidth: 264,
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
              {...(external
                ? { href: url, target: "_blank", rel: "noopener noreferrer" }
                : { component: RouterLink, to: url })}
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
              {external && (
                <img
                  alt="Opens in a new tab"
                  height={16}
                  src={ICONS.externalLinkIcon}
                  width={16}
                />
              )}
            </Link>
          </Card>
        ))}
      </Box>
    </div>
  );
};
