import { Box } from "@mui/material";
import { Link } from "react-router";

import { ds } from "@/consts/designTokens";
import type { PageTab } from "@/hooks/useGovernanceActionTabs";

type PageTabsProps = {
  tabs: PageTab[];
  /** Names the navigation, e.g. "Governance Actions". */
  label?: string;
};

export const PageTabs = ({ tabs, label }: PageTabsProps) => (
  <Box
    aria-label={label}
    component="nav"
    sx={{
      // The row's line is an inset shadow, so the active tab's underline sits
      // inside the row and overflowY can stay hidden (no stray scrollbar).
      boxShadow: `inset 0 -1px 0 ${ds.gray["200"]}`,
      display: "flex",
      overflowX: "auto",
      overflowY: "hidden",
    }}
  >
    {tabs.map((tab) => (
      <Box
        key={tab.to}
        aria-current={tab.isActive ? "page" : undefined}
        component={Link}
        data-testid={tab.dataTestId}
        to={tab.to}
        sx={{
          borderBottom: `1px solid ${
            tab.isActive ? ds.primary["600"] : "transparent"
          }`,
          boxSizing: "border-box",
          color: tab.isActive ? ds.primary["600"] : ds.gray["500"],
          fontSize: 14,
          fontWeight: tab.isActive ? 600 : 400,
          height: 38,
          letterSpacing: "-0.09px",
          lineHeight: "20px",
          px: 1.5,
          py: 1,
          textDecoration: "none",
          whiteSpace: "nowrap",
          "&:hover": {
            color: tab.isActive ? ds.primary["600"] : ds.gray["900"],
          },
        }}
      >
        {tab.label}
      </Box>
    ))}
  </Box>
);
