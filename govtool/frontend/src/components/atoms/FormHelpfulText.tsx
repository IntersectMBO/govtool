import { Typography } from "@mui/material";

import { FormHelpfulTextProps } from "./types";

export const FormHelpfulText = ({
  dataTestId,
  helpfulText,
  helpfulTextStyle,
  sx,
}: FormHelpfulTextProps) =>
  helpfulText && (
    <Typography
      color="#9792B5"
      data-testid={
        dataTestId ?? `${helpfulText.replace(/\s+/g, "-").toLowerCase()}-error`
      }
      fontSize={12}
      fontWeight={400}
      sx={{ mt: 0.5, ...sx }}
      {...helpfulTextStyle}
    >
      {helpfulText}
    </Typography>
  );
