import { Box, Link } from "@mui/material";

import { useTranslation } from "@hooks";
import { Typography } from "../atoms";
import { ICONS } from "@/consts";
import { Card } from "../molecules";

const LINKS = {
  ccPortal: {
    url: "https://constitution.gov.tools",
  },
  intersectWebsite: {
    url: "https://www.intersectmbo.org/",
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
        {Object.entries(LINKS).map(([key, { url }]) => (
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
