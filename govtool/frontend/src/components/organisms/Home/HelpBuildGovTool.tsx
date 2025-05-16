import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";

import I18n from "@/i18n";
import { GOVTOOL_URLS } from "@/consts";

import { Typography } from "../../atoms";
import { HomeCard } from "../../molecules/HomeCard";
import { openInNewTab } from "@/utils";

const GOVTOOL_CARD_LINKS = [
  {
    title: I18n.t("home.helpBuildGovTool.cards.githubRepo.title"),
    description: I18n.t("home.helpBuildGovTool.cards.githubRepo.description"),
    url: GOVTOOL_URLS.githubRepo,
  },
  {
    title: I18n.t("home.helpBuildGovTool.cards.documentation.title"),
    description: I18n.t(
      "home.helpBuildGovTool.cards.documentation.description",
    ),
    url: GOVTOOL_URLS.documentation,
  },
  {
    title: I18n.t("home.helpBuildGovTool.cards.telegram.title"),
    description: I18n.t("home.helpBuildGovTool.cards.telegram.description"),
    url: GOVTOOL_URLS.telegram,
  },
];

export const HelpBuildGovTool = () => {
  const { t } = useTranslation();

  const handleCardClick = ({ url }: { url?: string }) => {
    if (!url) return;
    openInNewTab(url);
  };

  return (
    <Box my={4} component="section" data-testid="help-build-govtool-section">
      <Typography variant="title2">
        {t("home.helpBuildGovTool.section.title")}
      </Typography>
      <Box
        display="grid"
        gridTemplateColumns={{
          xxs: "repeat(1, 1fr)",
          sm: "repeat(2, 1fr)",
          lg: "repeat(3, 1fr)",
        }}
        gap={4}
        mt={4}
      >
        {GOVTOOL_CARD_LINKS.map(({ title, description, url }) => (
          <HomeCard
            key={title}
            title={title}
            description={description}
            onCardClick={() => handleCardClick({ url })}
          />
        ))}
      </Box>
    </Box>
  );
};
