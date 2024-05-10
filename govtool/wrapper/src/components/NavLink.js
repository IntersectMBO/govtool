import Link from "next/link";

import { Typography } from "@mui/material";

export const NavLink = ({ label, to }) => {
  const isActive = window.location.pathname === to;

  return (
    <Link href={to} style={{ textDecoration: "none" }}>
      <Typography
        variant={isActive ? "bodySemiM" : "titleS"}
        sx={{
          color: isActive ? "orange.500" : "black",
          "&:hover": { color: "orange.500" },
        }}
      >
        {label}
      </Typography>
    </Link>
  );
};
