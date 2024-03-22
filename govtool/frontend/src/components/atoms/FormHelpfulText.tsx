import { Typography } from "@mui/material";

import { FormHelpfulTextProps } from "./types";

export const FormHelpfulText = ({
  helpfulText,
  helpfulTextStyle,
}: FormHelpfulTextProps) =>
  helpfulText && (
    <Typography
      color="#9792B5"
      data-testid={`${helpfulText.replace(/\s+/g, "-").toLowerCase()}-error`}
      fontSize={12}
      fontWeight={400}
      sx={{ mt: 0.5 }}
      {...helpfulTextStyle}
    >
      {helpfulText}
    </Typography>
  );
