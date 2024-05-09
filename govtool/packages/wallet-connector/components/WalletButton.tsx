import { Box, CircularProgress, Typography } from "@mui/material";

import { WalletButtonProps } from "./types";

export const WalletButton = ({
  dataTestId,
  icon,
  isLoading,
  label,
  name,
  onClick,
}: WalletButtonProps) => {
  return (
    <Box
      data-testid={dataTestId}
      sx={[
        style.container,
        {
          border: isLoading ? "none" : `1px solid #D6E2FF`,
          bgcolor: isLoading ? "#EAE9F0" : "white",
          boxShadow: isLoading ? undefined : "0px 0px 11px 0px #24223230",
          cursor: isLoading ? "default" : "pointer",
          "&:hover": isLoading
            ? undefined
            : {
                background: "#D6E2FF",
              },
        },
      ]}
      key={name}
      onClick={onClick}
    >
      <img
        alt={`${name} icon`}
        src={icon}
        style={{
          ...style.image,
          filter: isLoading ? "grayscale(100%)" : "none",
        }}
      />
      <Typography
        sx={{ ...style.label, color: isLoading ? "#C1BED3" : "primary" }}
      >
        {name ?? label}
      </Typography>
      <div style={style.spacer} />
      {isLoading && (
        <Box sx={style.loadingContainer}>
          <CircularProgress size={26} />
        </Box>
      )}
    </Box>
  );
};

const style = {
  container: {
    alignItems: "center",
    borderRadius: "100px",
    boxSizing: "border-box",
    display: "flex",
    justifyContent: "space-between",
    marginBottom: "16px",
    padding: "12px 13px 12px 13px",
    transition: "background .2s",
    position: "relative",
    width: "100%",
  },
  image: {
    height: "24px",
    width: "24px",
  },
  label: {
    fontSize: "16px",
    fontWeight: "500",
  },
  loadingContainer: {
    display: "flex",
    justifyContent: "center",
    left: 0,
    position: "absolute",
    right: 0,
  },
  spacer: { height: "24px", width: "24px" },
};
