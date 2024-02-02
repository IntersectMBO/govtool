import { Button, CircularProgress } from "@mui/material";
import type { ButtonProps, SxProps } from "@mui/material";

interface Props extends Omit<ButtonProps, "size" | "sx"> {
  isLoading?: boolean;
  size?: "small" | "medium" | "large" | "extraLarge";
  sx?: SxProps;
}

export const LoadingButton = ({
  isLoading,
  disabled,
  children,
  size = "large",
  sx,
  ...rest
}: Props) => {
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
};
