import { useId } from "react";
import { Control, Controller } from "react-hook-form";
import { Box, InputBase, InputBaseProps } from "@mui/material";

import { Typography } from ".";

interface Props extends InputBaseProps {
  control: Control<any>;
  formFieldName: string;
  label?: string;
  errorMessage?: string;
  dataTestId?: string;
  placeholder?: string;
  width?: string;
  marginTop?: string;
  marginBottom?: string;
}

export const Input = ({
  control,
  formFieldName,
  errorMessage,
  label,
  placeholder,
  disabled,
  type = "text",
  width,
  marginTop,
  marginBottom,
  dataTestId,
  ...rest
}: Props) => {
  const id = useId();

  return (
    <Controller
      control={control}
      name={formFieldName}
      render={({ field: { onChange } }) => (
        <Box
          width={width}
          display="flex"
          flexDirection="column"
          sx={{ marginTop, marginBottom }}
        >
          {label && (
            <Typography
              variant="caption"
              sx={{ height: "20px", marginBottom: "5px", fontSize: "16px" }}
            >
              {label}
            </Typography>
          )}
          <InputBase
            inputProps={{ "data-testid": dataTestId }}
            onChange={onChange}
            disabled={disabled}
            id={id}
            placeholder={placeholder}
            type={type}
            sx={{
              border: 1,
              bgcolor: "white",
              padding: "8px 16px",
              backgroundColor: errorMessage ? "inputRed" : "transparent",
              borderColor: errorMessage ? "red" : "secondaryBlue",
              borderRadius: 50,
              width: "100%",
            }}
            {...rest}
          />
          <Box sx={{ height: "20px" }}>
            {errorMessage && (
              <Typography
                variant="caption"
                color="red"
                sx={{ height: "20px", marginTop: "2px" }}
                data-testid={`${errorMessage
                  .replace(/\s+/g, "-")
                  .toLowerCase()}-error`}
              >
                {errorMessage}
              </Typography>
            )}
          </Box>
        </Box>
      )}
    />
  );
};
