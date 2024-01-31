import { Box, Typography } from "@mui/material";
import { FC } from "react";

import { useCardano } from "@context";
import { theme } from "@/theme";
import { useNavigate } from "react-router-dom";
import { PATHS } from "@/consts";

export interface WalletOption {
  icon: string;
  label: string;
  name: string;
  cip95Available: boolean;
  dataTestId?: string;
}

export const WalletOptionButton: FC<WalletOption> = ({ ...props }) => {
  const { enable } = useCardano();
  const {
    palette: { lightBlue },
  } = theme;
  const navigate = useNavigate();

  const { dataTestId, icon, label, name, cip95Available } = props;

  return (
    <Box
      data-testid={dataTestId}
      sx={{
        alignItems: "center",
        border: `1px solid ${lightBlue}`,
        borderRadius: "100px",
        boxShadow: "0px 0px 11px 0px #24223230",
        boxSizing: "border-box",
        cursor: cip95Available ? "pointer" : "unset",
        display: "flex",
        justifyContent: "space-between",
        marginBottom: "16px",
        padding: "12px 28px 12px 13px",
        transition: "background .2s",
        width: "100%",
        "&:hover": {
          background: lightBlue,
        },
      }}
      key={name}
      onClick={async () => {
        const result = await enable(name);
        if (result?.stakeKey) {
          navigate(PATHS.dashboard);
          return;
        }
        navigate(PATHS.stakeKeys);
      }}
    >
      <img
        alt={`${name} icon`}
        src={icon}
        style={{ height: "24px", width: "24px" }}
      />
      <Typography
        color="primaryBlue"
        sx={{ fontSize: "16px", fontWeight: "500" }}
      >
        {name ?? label}
      </Typography>
      <img
        alt={`${name} icon`}
        src={icon}
        style={{ height: "24px", width: "24px", visibility: "hidden" }}
      />
    </Box>
  );
};
