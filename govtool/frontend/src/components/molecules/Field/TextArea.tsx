import { Box } from "@mui/material";

import {
  FormErrorMessage,
  FormHelpfulText,
  TextArea as TextAreaBase,
  Typography,
} from "@atoms";

import { TextAreaFieldProps } from "./types";

export const TextArea = ({
  errorMessage,
  errorStyles,
  helpfulText,
  helpfulTextStyle,
  label,
  labelStyles,
  layoutStyles,
  maxLength = 500,
  ...props
}: TextAreaFieldProps) => {
  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        width: "100%",
        position: "relative",
        ...layoutStyles,
      }}
    >
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
      <TextAreaBase maxLength={maxLength} {...props} />
      <FormHelpfulText
        helpfulText={helpfulText}
        helpfulTextStyle={helpfulTextStyle}
      />
      <FormErrorMessage errorMessage={errorMessage} errorStyles={errorStyles} />
      <Typography
        color="#8E908E"
        sx={{ bottom: 35, position: "absolute", right: 15 }}
        variant="caption"
      >
        {props?.value?.toString().length}/{maxLength}
      </Typography>
    </Box>
  );
};
