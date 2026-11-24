import { useId, useState } from "react";
import { Box } from "@mui/material";
import { Minus, Plus } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds } from "@/consts/designTokens";
import { LINKS } from "@/consts/links";

import { DisplayHeading, Eyebrow, HomeColumn, Lead } from "./homeParts";

const ITEMS = [
  "whatIsGovTool",
  "needWallet",
  "whatIsDRep",
  "whatIsGovernance",
  "whoCanParticipate",
  "delegateVsDirect",
  "needAda",
  "whatCanIVote",
];

const FaqItem = ({
  itemKey,
  isOpen,
  onToggle,
}: {
  itemKey: string;
  isOpen: boolean;
  onToggle: () => void;
}) => {
  const { t } = useTranslation();
  const id = useId();

  return (
    <Box sx={{ borderBottom: `1px solid ${ds.gray["200"]}` }}>
      <Box
        aria-controls={`${id}-answer`}
        aria-expanded={isOpen}
        component="button"
        data-testid={`faq-${itemKey}`}
        id={`${id}-question`}
        onClick={onToggle}
        type="button"
        sx={{
          alignItems: "center",
          background: "none",
          border: "none",
          color: ds.gray["900"],
          cursor: "pointer",
          display: "flex",
          fontFamily: "inherit",
          fontSize: 14,
          fontWeight: isOpen ? 600 : 400,
          gap: 2,
          justifyContent: "space-between",
          letterSpacing: "-0.09px",
          lineHeight: "20px",
          py: 2.5,
          textAlign: "left",
          width: "100%",
        }}
      >
        {t(`landing.faq.items.${itemKey}.q`)}
        <Box
          component="span"
          sx={{
            alignItems: "center",
            backgroundColor: isOpen ? ds.primary["600"] : ds.gray["600"],
            borderRadius: "50%",
            color: "#FFFFFF",
            display: "flex",
            flexShrink: 0,
            height: 16,
            justifyContent: "center",
            width: 16,
          }}
        >
          {isOpen ? (
            <Minus aria-hidden size={12} strokeWidth={3} />
          ) : (
            <Plus aria-hidden size={12} strokeWidth={3} />
          )}
        </Box>
      </Box>
      {isOpen && (
        <Box aria-labelledby={`${id}-question`} id={`${id}-answer`} role="region" sx={{ pb: 2.5, pr: 4 }}>
          <Lead>{t(`landing.faq.items.${itemKey}.a`)}</Lead>
        </Box>
      )}
    </Box>
  );
};

/** Common questions, the first one open. */
export const HomeFaq = () => {
  const { t } = useTranslation();
  const [openKey, setOpenKey] = useState<string | null>(ITEMS[0]);

  return (
    <Box
      component="section"
      sx={{
        background:
          "radial-gradient(50% 60% at 15% 40%, rgba(223, 234, 255, 0.4) 0%, rgba(223, 234, 255, 0) 100%)",
        py: { xxs: 6, md: 10 },
      }}
    >
      <HomeColumn sx={{ display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: { xxs: 4, md: 10 } }}>
        <Box sx={{ flexShrink: 0, maxWidth: 260 }}>
          <Eyebrow>{t("landing.faq.eyebrow")}</Eyebrow>
          <DisplayHeading sx={{ mt: 2 }}>{t("landing.faq.title")}</DisplayHeading>
          <Lead sx={{ mt: 2 }}>{t("landing.faq.aside")}</Lead>
        </Box>
        <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}`, flex: 1 }}>
          {ITEMS.map((itemKey) => (
            <FaqItem
              key={itemKey}
              isOpen={openKey === itemKey}
              itemKey={itemKey}
              onToggle={() => setOpenKey(openKey === itemKey ? null : itemKey)}
            />
          ))}
          <Box
            component="a"
            data-testid="home-faq-read-more"
            href={LINKS.FAQS}
            rel="noopener noreferrer"
            target="_blank"
            sx={{ color: ds.primary["600"], display: "inline-block", fontSize: 14, fontWeight: 600, mt: 3, textDecoration: "none" }}
          >
            {`${t("landing.faq.readMore")} ↗`}
          </Box>
        </Box>
      </HomeColumn>
    </Box>
  );
};
