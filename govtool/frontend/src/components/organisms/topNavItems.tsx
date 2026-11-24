import { Link as RouterLink } from "react-router";
import { Box } from "@mui/material";

import { PATHS, PDF_PATHS } from "@/consts/paths";
import { ds } from "@/consts/designTokens";

export type NavEntry = {
  dataTestId: string;
  label: string;
  navTo?: string;
  newTabLink?: string | null;
};

const GOVERNANCE_ACTION_IDS = ["governance-actions", "governance-actions-link"];

export const isNavEntryActive = (entry: NavEntry, pathname: string) => {
  if (GOVERNANCE_ACTION_IDS.includes(entry.dataTestId)) {
    return [PATHS.governanceActions, PDF_PATHS.proposalDiscussion].some(
      (path) => pathname === path || pathname.startsWith(`${path}/`),
    );
  }
  if (!entry.navTo) return false;
  if (entry.navTo === PATHS.home) return pathname === PATHS.home;
  return pathname === entry.navTo || pathname.startsWith(`${entry.navTo}/`);
};

const pillSx = (isActive: boolean) => ({
  alignItems: "center",
  backgroundColor: isActive ? "#FF640A14" : "transparent",
  border: `1px solid ${isActive ? "#FF640A29" : "transparent"}`,
  borderRadius: "30px",
  color: isActive ? ds.orange["750"] : ds.gray["500"],
  display: "inline-flex",
  fontSize: 14,
  fontWeight: isActive ? 600 : 400,
  height: 28,
  letterSpacing: "-0.09px",
  lineHeight: "20px",
  px: "13px",
  textDecoration: "none",
  whiteSpace: "nowrap",
  "&:hover": { color: isActive ? ds.orange["750"] : ds.gray["900"] },
});

const drawerSx = (isActive: boolean) => ({
  backgroundColor: isActive ? "#FF640A14" : "transparent",
  borderRadius: "8px",
  color: isActive ? ds.orange["750"] : ds.gray["700"],
  display: "block",
  fontSize: 16,
  fontWeight: 500,
  letterSpacing: "-0.18px",
  lineHeight: "24px",
  px: 2,
  py: 1.5,
  textDecoration: "none",
});

type NavEntryLinkProps = {
  entry: NavEntry;
  isActive: boolean;
  variant?: "bar" | "drawer";
  onClick?: (event: React.MouseEvent<HTMLElement>) => void;
};

/** One nav item: a pill in the top bar or a row in the mobile drawer. */
export const NavEntryLink = ({
  entry,
  isActive,
  variant = "bar",
  onClick,
}: NavEntryLinkProps) => {
  const sx = variant === "bar" ? pillSx(isActive) : drawerSx(isActive);

  if (entry.newTabLink) {
    return (
      <Box
        component="a"
        data-testid={entry.dataTestId}
        href={entry.newTabLink}
        onClick={onClick}
        rel="noopener noreferrer"
        sx={sx}
        target="_blank"
      >
        {entry.label}
      </Box>
    );
  }

  return (
    <Box
      aria-current={isActive ? "page" : undefined}
      component={RouterLink}
      data-testid={entry.dataTestId}
      onClick={onClick}
      sx={sx}
      to={entry.navTo ?? PATHS.home}
    >
      {entry.label}
    </Box>
  );
};
