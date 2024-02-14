import { Box } from "@mui/material";

import { FormErrorMessage, Input as InputBase, Typography } from "@atoms";

import { InputFieldProps } from "./types";

export const Input = ({
  errorMessage,
  errorStyles,
  label,
  labelStyles,
  layoutStyles,
  ...rest
}: InputFieldProps) => {
  return (
    <Box sx={{ width: "100%", ...layoutStyles }}>
      {label && (
        <Typography fontWeight={400} variant="body2" {...labelStyles}>
          {label}
        </Typography>
      )}
      <InputBase errorMessage={errorMessage} {...rest} />
      <FormErrorMessage errorMessage={errorMessage} errorStyles={errorStyles} />
    </Box>
  );
};
