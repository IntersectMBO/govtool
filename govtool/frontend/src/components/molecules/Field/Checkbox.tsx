import { Box } from "@mui/material";

import { Checkbox as CheckboxBase, Typography } from "@atoms";

import { CheckboxFieldProps } from "./types";
import { FormErrorMessage } from "@/components/atoms/FormErrorMessage";

export const Checkbox = ({
  errorMessage,
  errorStyles,
  label,
  labelStyles,
  layoutStyles,
  onChange,
  ...rest
}: CheckboxFieldProps) => {
  return (
    <Box sx={{ width: "100%", ...layoutStyles }}>
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          flexDirection: "row",
        }}
      >
        <CheckboxBase errorMessage={errorMessage} {...rest} />
        {label && (
          <Typography variant="caption" {...labelStyles}>
            {label}
          </Typography>
        )}
      </Box>
      <FormErrorMessage errorMessage={errorMessage} errorStyles={errorStyles} />
    </Box>
  );
};
