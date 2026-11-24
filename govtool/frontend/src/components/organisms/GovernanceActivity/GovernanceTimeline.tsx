import { Box } from "@mui/material";
import { Check, Flag, GitBranch } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds, RADIUS } from "@/consts/designTokens";

import { ActivityCard } from "./activityParts";

const STAGES = ["submitted", "voting", "ratified", "enacted"] as const;

/** The stages every governance action moves through, as a static explainer. */
export const GovernanceTimeline = () => {
  const { t } = useTranslation();

  return (
    <ActivityCard dataTestId="governance-timeline" sx={{ display: "flex", flexDirection: "column", gap: 3, p: { xxs: 2, md: 3 } }}>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1 }}>
        <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, letterSpacing: "-0.33px", lineHeight: "30px", m: 0 }}>
          {t("activity.timeline.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["500"], fontSize: 13, letterSpacing: "-0.04px", lineHeight: "19.5px", m: 0 }}>
          {t("activity.timeline.description")}
        </Box>
      </Box>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
        <Box
          component="ol"
          sx={{
            display: "grid",
            gridTemplateColumns: `repeat(${STAGES.length}, minmax(0, 1fr))`,
            listStyle: "none",
            m: 0,
            p: 0,
            position: "relative",
            // The dashed track between the first and last marker centres.
            "&::before": {
              borderTop: `1px dashed ${ds.primary["600"]}`,
              content: '""',
              left: `${100 / (STAGES.length * 2)}%`,
              position: "absolute",
              right: `${100 / (STAGES.length * 2)}%`,
              top: 15,
            },
          }}
        >
          {STAGES.map((stage) => (
            <Box
              key={stage}
              component="li"
              sx={{ alignItems: "center", display: "flex", flexDirection: "column", gap: 1, minWidth: 0, position: "relative" }}
            >
              <Box
                aria-hidden
                sx={{
                  alignItems: "center",
                  backgroundColor: ds.primary["600"],
                  borderRadius: "50%",
                  color: "#FFFFFF",
                  display: "flex",
                  height: 30,
                  justifyContent: "center",
                  width: 30,
                }}
              >
                {stage === "enacted" ? <Flag size={14} strokeWidth={2} /> : <Check size={14} strokeWidth={2.25} />}
              </Box>
              <Box
                component="span"
                sx={{ color: ds.gray["900"], fontSize: 13, fontWeight: 600, letterSpacing: "-0.04px", lineHeight: "19.5px", maxWidth: "100%", textAlign: "center" }}
              >
                {t(`activity.timeline.${stage}`)}
              </Box>
            </Box>
          ))}
        </Box>
        <Box
          component="p"
          sx={{
            alignItems: "center",
            backgroundColor: ds.gray["100"],
            borderRadius: `${RADIUS.sm}px`,
            // 600 rather than the design's 500 (4.5:1 here) for 12px text.
            color: ds.gray["600"],
            display: "flex",
            fontSize: 12,
            gap: 1,
            letterSpacing: "0.01em",
            lineHeight: "18px",
            m: 0,
            px: 1.75,
            py: 1.125,
          }}
        >
          <GitBranch aria-hidden size={16} strokeWidth={1.25} style={{ flexShrink: 0 }} />
          {t("activity.timeline.alternate")}
        </Box>
      </Box>
    </ActivityCard>
  );
};
