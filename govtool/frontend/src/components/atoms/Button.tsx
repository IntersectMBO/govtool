import { CircularProgress, Button as MUIButton } from "@mui/material";
import { ButtonProps } from ".";

export const Button = ({
  size = "large",
  variant = "contained",
  sx,
  isLoading,
  ...props
}: ButtonProps) => {
  const buttonHeight = {
    extraLarge: 48,
    large: 40,
    medium: 36,
    small: 32,
  }[size];

  const buttonPaddingHorizontal = {
    extraLarge: 3.5,
    large: 3.25,
    medium: 3,
    small: 3,
  }[size];

  return (
    <MUIButton
      disabled={isLoading || props?.disabled}
      sx={{
        fontSize: size === "extraLarge" ? 16 : 14,
        height: buttonHeight,
        px: buttonPaddingHorizontal,
        whiteSpace: "nowrap",
        ...sx,
      }}
      variant={variant}
      {...props}
    >
      {isLoading && (
        <CircularProgress size={26} sx={{ position: "absolute" }} />
      )}
      {props.children}
    </MUIButton>
  );
};
