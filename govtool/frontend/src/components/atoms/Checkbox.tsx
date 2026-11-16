import { useId } from "react";
import { Checkbox as MUICheckbox } from "@mui/material";

import { CheckboxProps } from "./types";

import { ds } from "@/consts/designTokens";

export const Checkbox = ({
  dataTestId,
  errorMessage,
  sx,
  ...props
}: CheckboxProps) => {
  const id = useId();

  return (
    <MUICheckbox
      id={id}
      inputProps={
        {
          "data-testid": dataTestId,
        } as React.InputHTMLAttributes<HTMLInputElement>
      }
      sx={{
        "& .MuiSvgIcon-root": { fontSize: 18 },
        color: errorMessage ? "red" : ds.primary["600"],
        ...sx,
      }}
      {...props}
    />
  );
};
