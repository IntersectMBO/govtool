import { Box } from "@mui/material";

import { Checkbox as CheckboxBase, FormErrorMessage, Typography } from "@atoms";

import { useCallback } from "react";
import { CheckboxFieldProps } from "./types";

export const Checkbox = ({
  errorMessage,
  errorStyles,
  label,
  labelStyles,
  layoutStyles,
  onChange,
  value,
  ...rest
}: CheckboxFieldProps) => {
  const handleValue = useCallback(() => {
    onChange(!value);
  }, [value]);

  return (
    <Box sx={{ width: "100%", ...layoutStyles }}>
      <Box
        onClick={handleValue}
        sx={{
          alignItems: "center",
          cursor: "pointer",
          display: "flex",
          flexDirection: "row",
          width: "fit-content",
        }}
      >
        <CheckboxBase
          {...{ onChange, value }}
          errorMessage={errorMessage}
          {...rest}
        />
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
