import Link from "next/link";
import Image from "next/image";

import { Typography } from "@mui/material";

export const DrawerNavLink = ({ activeIcon, icon, label, to, style }) => {
  const isActive = window.location.pathname === to;

  return (
    <Link
      href={to}
      style={{
        ...insideStyle.container,
        backgroundColor: isActive ? "#C2EFF299" : undefined,
        ...style,
      }}
    >
      {icon && (
        <Image
          alt="Sidebar icon"
          src={isActive ? activeIcon ?? icon : icon}
          style={insideStyle.image}
        />
      )}
      <Typography variant={isActive ? "bodyMediumM" : "bodyRegularM"}>
        {label}
      </Typography>
    </Link>
  );
};

const insideStyle = {
  container: {
    alignItems: "center",
    display: "flex",
    textDecoration: "none",
    height: "36px",
    borderRadius: "100px",
    padding: "8px 16px 8px 16px",
    boxSizing: "border-box",
  },
  image: { height: "20px", marginRight: "12px", width: "20px" },
};
