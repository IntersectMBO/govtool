import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";

import I18n from "@/i18n";
import { SOCIAL_MEDIA_URLS } from "@/consts";

import { Typography } from "../../atoms";
import { HomeCard } from "../../molecules/HomeCard";
import { openInNewTab } from "@/utils";

const SOCIAL_CARDS = [
  {
    title: I18n.t("home.joinDiscussion.cards.discord.title"),
    description: I18n.t("home.joinDiscussion.cards.discord.description"),
    url: SOCIAL_MEDIA_URLS.discord,
  },
  {
    title: I18n.t("home.joinDiscussion.cards.x.title"),
    description: I18n.t("home.joinDiscussion.cards.x.description"),
    url: SOCIAL_MEDIA_URLS.x,
  },
  {
    title: I18n.t("home.joinDiscussion.cards.telegram.title"),
    description: I18n.t("home.joinDiscussion.cards.telegram.description"),
    url: SOCIAL_MEDIA_URLS.telegram,
  },
];

export const Socials = () => {
  const { t } = useTranslation();

  const handleCardClick = ({ url }: { url?: string }) => {
    if (!url) return;
    openInNewTab(url);
  };

  return (
    <Box my={4} component="section" data-testid="join-discussion-section">
      <Typography variant="title2">
        {t("home.joinDiscussion.section.title")}
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
        {SOCIAL_CARDS.map(({ title, description, url }) => (
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
