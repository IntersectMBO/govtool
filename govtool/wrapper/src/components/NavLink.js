import Link from "next/link";

import { Typography } from "@mui/material";

export const NavLink = ({
  label,
  to,
  activeTypographyVariant,
  typographyVariant,
}) => {
  const isActive = window.location.pathname === to;

  return (
    <Link href={to} style={{ textDecoration: "none" }}>
      <Typography
        variant={
          isActive
            ? activeTypographyVariant ?? "bodySemiM"
            : typographyVariant ?? "titleS"
        }
        sx={{
          color: isActive ? "orange.500" : undefined,
          "&:hover": { color: "orange.500" },
        }}
      >
        {label}
      </Typography>
    </Link>
  );
};
