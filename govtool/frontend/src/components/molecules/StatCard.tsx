import { ReactNode } from "react";
import { Box } from "@mui/material";

import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

type Props = {
  label: string;
  value: ReactNode;
  caption?: ReactNode;
  valueColor?: string;
  icon?: ReactNode;
  dataTestId?: string;
};

/** A small figure card: label, a large value and an optional caption. */
export const StatCard = ({
  label,
  value,
  caption,
  valueColor = ds.gray["900"],
  icon,
  dataTestId,
}: Props) => (
  <Box
    data-testid={dataTestId}
    sx={{
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      boxShadow: SHADOWS.card,
      display: "flex",
      flexDirection: "column",
      gap: 0.5,
      minWidth: 0,
      p: 2.25,
    }}
  >
    <Box
      sx={{
        alignItems: "flex-start",
        color: ds.gray["600"],
        display: "flex",
        fontSize: 13,
        justifyContent: "space-between",
        lineHeight: "18px",
      }}
    >
      {label}
      {icon}
    </Box>
    <Box
      sx={{
        color: valueColor,
        fontSize: 22,
        fontWeight: 500,
        letterSpacing: "-0.4px",
        lineHeight: "30px",
        overflowWrap: "anywhere",
      }}
    >
      {value}
    </Box>
    {caption && (
      <Box sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px" }}>
        {caption}
      </Box>
    )}
  </Box>
);
