import { ReactNode } from "react";
import { Box } from "@mui/material";
import { Info } from "lucide-react";

import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";

// The pieces the create governance action steps share.

export const StepHeading = ({ title, body }: { title: string; body?: ReactNode }) => (
  <Box>
    <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 24, md: 28 }, fontWeight: 600, letterSpacing: "-0.5px", m: 0 }}>
      {title}
    </Box>
    {body && (
      <Box component="p" sx={{ color: ds.gray["600"], fontSize: 15, lineHeight: "22px", m: 0, maxWidth: 640, mt: 1.25 }}>
        {body}
      </Box>
    )}
  </Box>
);

export const SectionTag = ({ label, tone }: { label: string; tone: "error" | "neutral" }) => (
  <Box
    component="span"
    sx={{
      backgroundColor: PILL_TONES[tone].bg,
      borderRadius: `${RADIUS.xs}px`,
      color: PILL_TONES[tone].text,
      fontSize: 11,
      fontWeight: 600,
      letterSpacing: "0.3px",
      px: 0.75,
      py: 0.25,
      textTransform: "uppercase",
    }}
  >
    {label}
  </Box>
);

/** A white section card with a title row. */
export const SectionCard = ({
  title,
  tag,
  subtitle,
  children,
  testId,
}: {
  title?: string;
  tag?: ReactNode;
  subtitle?: string;
  children: ReactNode;
  testId?: string;
}) => (
  <Box
    component="section"
    data-testid={testId}
    sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, boxShadow: SHADOWS.card, display: "flex", flexDirection: "column", px: { xxs: 2.5, md: 4 }, py: { xxs: 2.5, md: 3.5 } }}
  >
    {title && (
      <Box sx={{ mb: subtitle ? 0.5 : 2.5 }}>
        <Box sx={{ alignItems: "center", display: "flex", gap: 1.25 }}>
          <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>{title}</Box>
          {tag}
        </Box>
      </Box>
    )}
    {subtitle && <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, m: 0, mb: 2.5 }}>{subtitle}</Box>}
    {children}
  </Box>
);

/** A blue info note. */
export const InfoNote = ({ children, title }: { children: ReactNode; title?: string }) => (
  <Box sx={{ backgroundColor: ds.primary["25"], border: `1px solid ${ds.primary["200"]}`, borderRadius: `${RADIUS.lg}px`, color: ds.primary["700"], display: "flex", fontSize: 14, gap: 1.25, lineHeight: "20px", px: 2, py: 1.5 }}>
    <Info aria-hidden size={16} style={{ flexShrink: 0, marginTop: 2 }} />
    <Box>
      {title && <Box sx={{ color: ds.gray["900"], fontWeight: 600, mb: 0.5 }}>{title}</Box>}
      <Box sx={{ color: title ? ds.gray["600"] : undefined }}>{children}</Box>
    </Box>
  </Box>
);
