import { Box, Skeleton } from "@mui/material";
import { ArrowUpRight } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds, FONT_FAMILY, RADIUS, SHADOWS } from "@/consts/designTokens";
import { LINKS, NEWSLETTER } from "@/consts/links";
import { useNewsletterPosts } from "@/hooks/queries/useNewsletterPosts";
import type { NewsItem } from "@/models/newsletter";

import { DisplayHeading, Eyebrow, HomeColumn } from "./homeParts";
import { NewsletterSignup } from "./NewsletterSignup";

const chipSx = {
  backgroundColor: ds.primary["50"],
  borderRadius: `${RADIUS.xs}px`,
  color: ds.primary["700"],
  fontSize: 12,
  fontWeight: 600,
  lineHeight: "18px",
  px: 0.75,
};

const rowGrid = {
  alignItems: "center",
  borderBottom: `1px dashed ${ds.gray["300"]}`,
  columnGap: { xxs: 2, md: 3 },
  display: "grid",
  gridTemplateColumns: { xxs: "48px minmax(0, 1fr) 40px", md: "72px minmax(0, 1fr) 40px" },
  py: 3,
};

const NewsRow = ({ item, index }: { item: NewsItem; index: number }) => {
  const { t } = useTranslation();
  return (
    <Box component="li" data-testid={`home-news-item-${index}`} sx={rowGrid}>
      <Box
        aria-hidden
        sx={{ color: ds.gray["900"], fontFamily: FONT_FAMILY.mono, fontSize: { xxs: 30, md: 42 }, lineHeight: 1 }}
      >
        {String(index + 1).padStart(2, "0")}
      </Box>
      <Box sx={{ minWidth: 0 }}>
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 0.75 }}>
          {item.month && <Box component="span" sx={chipSx}>{item.month}</Box>}
          {item.tag && <Box component="span" sx={chipSx}>{item.tag}</Box>}
        </Box>
        <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600, lineHeight: "22px", m: 0, mt: 1.25 }}>
          {item.title}
        </Box>
        {item.excerpt && (
          <Box
            component="p"
            sx={{
              color: ds.gray["600"],
              display: "-webkit-box",
              fontSize: 14,
              lineHeight: "20px",
              m: 0,
              maxWidth: 660,
              mt: 1,
              overflow: "hidden",
              WebkitBoxOrient: "vertical",
              WebkitLineClamp: 3,
            }}
          >
            {item.excerpt}
          </Box>
        )}
      </Box>
      <Box
        aria-label={t("landing.news.readPost", { title: item.title })}
        component="a"
        href={item.url}
        rel="noopener noreferrer"
        sx={{
          alignItems: "center",
          backgroundColor: "#FFFFFF",
          borderRadius: "50%",
          boxShadow: SHADOWS.card,
          color: ds.gray["900"],
          display: "flex",
          height: 40,
          justifyContent: "center",
          width: 40,
          "&:hover": { color: ds.primary["600"] },
          "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
        }}
        target="_blank"
      >
        <ArrowUpRight aria-hidden size={16} />
      </Box>
    </Box>
  );
};

/**
 * The newsletter's four latest posts, a link to the rest on the newsletter
 * site, and its sign-up. The sign-up lived in the footer before.
 */
export const HomeNews = () => {
  const { t } = useTranslation();
  const news = useNewsletterPosts();

  let list;
  if (news.isLoading) {
    list = [0, 1, 2].map((index) => (
      <Box key={index} component="li" sx={rowGrid}>
        <Skeleton height={42} variant="text" width={48} />
        <Box>
          <Skeleton variant="text" width="40%" />
          <Skeleton variant="text" width="80%" />
        </Box>
        <Skeleton height={40} variant="circular" width={40} />
      </Box>
    ));
  } else if (news.isError && !news.items.length) {
    list = (
      <Box component="li" data-testid="home-news-error" sx={{ color: ds.gray["600"], fontSize: 14, py: 3 }}>
        {t("landing.news.error")}
      </Box>
    );
  } else {
    list = news.items.map((item, index) => <NewsRow key={item.id} index={index} item={item} />);
  }

  return (
    <Box
      component="section"
      data-testid="home-news-section"
      sx={{ background: "linear-gradient(180deg, #F8F7F4 0%, #FAF7F3 100%)", py: { xxs: 6, md: 10 } }}
    >
      <HomeColumn>
        <Box sx={{ alignItems: { md: "flex-end" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
          <Box>
            <Eyebrow>{t("landing.news.eyebrow")}</Eyebrow>
            <DisplayHeading sx={{ mt: 2 }}>{t("landing.news.title")}</DisplayHeading>
          </Box>
          <Box
            component="a"
            data-testid="newsletter-subscribe-link"
            href={LINKS.NEWSLETTER_SIGNUP}
            rel="noopener noreferrer"
            sx={{
              alignItems: "center",
              alignSelf: { xxs: "flex-start", md: "center" },
              backgroundColor: ds.primary["50"],
              borderRadius: `${RADIUS.pill}px`,
              color: ds.primary["700"],
              display: "inline-flex",
              fontSize: 14,
              fontWeight: 600,
              gap: 0.5,
              height: 44,
              px: 2,
              textDecoration: "none",
              "&:hover": { backgroundColor: ds.primary["100"] },
              "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
            }}
            target="_blank"
          >
            {t("landing.news.subscribeCta")}
            <ArrowUpRight aria-hidden size={14} />
          </Box>
        </Box>
        <Box component="ol" sx={{ listStyle: "none", m: 0, mt: 3, p: 0 }}>
          {list}
        </Box>
        {!news.isLoading && (
          <Box sx={{ display: "flex", justifyContent: "center", mt: 4 }}>
            <Box
              component="a"
              data-testid="home-news-view-all"
              href={NEWSLETTER.SITE}
              rel="noopener noreferrer"
              sx={{
                alignItems: "center",
                backgroundColor: "#FFFFFF",
                border: `1px solid ${ds.gray["200"]}`,
                borderRadius: `${RADIUS.pill}px`,
                color: ds.gray["900"],
                display: "inline-flex",
                fontSize: 14,
                fontWeight: 600,
                gap: 0.5,
                height: 40,
                px: 2,
                textDecoration: "none",
                "&:hover": { backgroundColor: ds.gray["50"] },
                "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
              }}
              target="_blank"
            >
              {t("landing.news.viewAll")}
              <ArrowUpRight aria-hidden size={14} />
            </Box>
          </Box>
        )}
        <Box sx={{ mt: { xxs: 5, md: 6 } }}>
          <NewsletterSignup />
        </Box>
      </HomeColumn>
    </Box>
  );
};
