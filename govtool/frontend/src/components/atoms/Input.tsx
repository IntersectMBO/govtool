import { useId } from "react";
import { InputBase } from "@mui/material";

import { InputProps } from "./types";

export const Input = ({
  errorMessage,
  dataTestId,
  sx,
  ...rest
}: InputProps) => {
  const id = useId();

  return (
    <InputBase
      id={id}
      inputProps={{ "data-testid": dataTestId }}
      sx={{
        backgroundColor: errorMessage ? "inputRed" : "transparent",
        border: 1,
        borderColor: errorMessage ? "red" : "secondaryBlue",
        borderRadius: 50,
        padding: "8px 16px",
        width: "100%",
        ...sx,
      }}
      {...rest}
    />
  );
};
