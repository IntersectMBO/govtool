import type { GrowProps } from "@mui/material/Grow";
import Grow from "@mui/material/Grow";
import type { SnackbarProps } from "@mui/material/Snackbar";
import MuiSnackbar from "@mui/material/Snackbar";

import { theme } from "@/theme";
import type { SnackbarSeverity } from "@models";

function GrowTransition(props: GrowProps) {
  return <Grow {...props} />;
}

interface Props extends SnackbarProps {
  severity: SnackbarSeverity;
}

export function Snackbar({ severity, ...props }: Props) {
  return (
    <MuiSnackbar
      ContentProps={{
        sx: {
          display: "flex",
          justifyContent: "flex-start",
          background:
            severity === "success"
              ? theme.palette.positiveGreen
              : theme.palette.negativeRed,
        },
      }}
      sx={{
        "&.MuiSnackbar-root": {
          top: "48px",
        },
      }}
      TransitionComponent={GrowTransition}
      {...props}
    />
  );
}
