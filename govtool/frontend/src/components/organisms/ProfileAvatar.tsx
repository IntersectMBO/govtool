import { Box } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import { UserRound } from "lucide-react";

import { PATHS } from "@/consts/paths";
import { ds } from "@/consts/designTokens";

const initials = (name: string) =>
  name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((part) => part[0]?.toUpperCase() ?? "")
    .join("");

/** The connected user's round avatar in the top nav: their initials, or an icon. */
export const ProfileAvatar = ({ name }: { name?: string | null }) => {
  const { t } = useTranslation();
  const letters = name ? initials(name) : "";
  return (
    <Box
      aria-label={t("topNav.profile")}
      component={RouterLink}
      data-testid="profile-avatar"
      sx={{
        alignItems: "center",
        background: `linear-gradient(135deg, ${ds.orange["700"]} 0%, ${ds.orange["750"]} 100%)`,
        borderRadius: "50%",
        color: "#FFFFFF",
        display: "flex",
        flexShrink: 0,
        fontSize: 15,
        fontWeight: 600,
        height: 40,
        justifyContent: "center",
        textDecoration: "none",
        width: 40,
        "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
      }}
      to={PATHS.profile}
    >
      {letters || <UserRound aria-hidden size={18} />}
    </Box>
  );
};
