import { Button, CircularProgress } from "@mui/material";

import { LoadingButtonProps } from "./types";

export function LoadingButton({
  isLoading,
  disabled,
  children,
  size = "large",
  sx,
  ...rest
}: LoadingButtonProps) {
  const buttonHeight = {
    extraLarge: 48,
    large: 40,
    medium: 36,
    small: 32,
  }[size];

  return (
    <Button
      disabled={disabled || isLoading}
      sx={{ height: buttonHeight, ...sx }}
      {...rest}
    >
      {isLoading && (
        <CircularProgress size={26} sx={{ position: "absolute" }} />
      )}
      {children}
    </Button>
  );
}
