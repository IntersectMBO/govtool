import { Typography } from "@mui/material";

import { FormErrorMessageProps } from "./types";

export const FormErrorMessage = ({
  errorMessage,
  errorStyles,
}: FormErrorMessageProps) => {
  return (
    errorMessage && (
      <Typography
        color="red"
        data-testid={`${errorMessage.replace(/\s+/g, "-").toLowerCase()}-error`}
        fontSize={12}
        fontWeight={400}
        sx={{ mt: 0.25 }}
        {...errorStyles}
      >
        {errorMessage}
      </Typography>
    )
  );
};
