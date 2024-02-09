import { Box } from "@mui/material";

import { Input as InputBase, Typography } from "@atoms";

import { InputFieldProps } from "./types";

export const Input = ({
  errorMessage,
  errorStyle,
  label,
  labelStyle,
  layoutStyle,
  ...rest
}: InputFieldProps) => {
  return (
    <Box sx={{ width: "100%", ...layoutStyle }}>
      {label && (
        <Typography fontWeight={400} variant="body2" {...labelStyle}>
          {label}
        </Typography>
      )}
      <InputBase errorMessage={errorMessage} {...rest} />
      {errorMessage && (
        <Typography
          variant="caption"
          color="red"
          data-testid={`${errorMessage
            .replace(/\s+/g, "-")
            .toLowerCase()}-error`}
          sx={{ mt: 0.25 }}
          {...errorStyle}
        >
          {errorMessage}
        </Typography>
      )}
    </Box>
  );
};
