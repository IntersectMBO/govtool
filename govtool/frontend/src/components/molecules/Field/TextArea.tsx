import {
  FormErrorMessage,
  FormHelpfulText,
  TextArea as TextAreaBase,
  Typography,
} from "@atoms";

import { TextAreaFieldProps } from "./types";
import { Box } from "@mui/material";

export const TextArea = ({
  errorMessage,
  errorStyles,
  helpfulText,
  helpfulTextStyle,
  label,
  labelStyles,
  layoutStyles,
  ...props
}: TextAreaFieldProps) => {
  return (
    <Box sx={{ width: "100%", ...layoutStyles }}>
      {label && (
        <Typography
          fontWeight={400}
          sx={{ mb: 0.5 }}
          variant="body2"
          {...labelStyles}
        >
          {label}
        </Typography>
      )}
      <TextAreaBase {...props} />
      <FormHelpfulText
        helpfulText={helpfulText}
        helpfulTextStyle={helpfulTextStyle}
      />
      <FormErrorMessage errorMessage={errorMessage} errorStyles={errorStyles} />
    </Box>
  );
};
