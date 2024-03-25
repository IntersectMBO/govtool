import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { theme } from "@/theme";

import { StepProps } from "./types";

export const Step = ({
  component,
  componentsLayoutStyles,
  label,
  layoutStyles,
  stepNumber,
}: StepProps) => {
  const {
    palette: { boxShadow2 },
  } = theme;

  return (
    <Box
      sx={{
        flexDirection: "row",
        display: "flex",
        width: "100%",
        ...layoutStyles,
      }}
    >
      <Box
        sx={{
          alignItems: "center",
          borderRadius: "100%",
          boxShadow: `2px 2px 20px 0px ${boxShadow2}`,
          display: "flex",
          height: 54,
          justifyContent: "center",
          width: 54,
        }}
      >
        <Typography color="primary" fontWeight={400} variant="title2">
          {stepNumber}
        </Typography>
      </Box>

      <Box
        sx={{
          display: "flex",
          flex: 1,
          flexDirection: "column",
          ml: 3,
          ...componentsLayoutStyles,
        }}
      >
        <Typography fontWeight={500} variant="body1">
          {label}
        </Typography>
        {component}
      </Box>
    </Box>
  );
};
