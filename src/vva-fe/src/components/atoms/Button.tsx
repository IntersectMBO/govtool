import {
  Button as MUIButton,
  ButtonProps as MUIButtonProps,
  SxProps,
} from "@mui/material";

interface ButtonProps extends Omit<MUIButtonProps, "size" | "sx" | "variant"> {
  size?: "small" | "medium" | "large" | "extraLarge";
  variant?: "contained" | "outlined" | "text";
  sx?: SxProps;
}

export const Button = ({
  size = "large",
  variant = "contained",
  sx,
  ...props
}: ButtonProps) => {
  const buttonHeight = {
    extraLarge: 48,
    large: 40,
    medium: 36,
    small: 32,
  }[size];

  return (
    <MUIButton
      sx={{
        fontSize: size === "extraLarge" ? 16 : 14,
        height: buttonHeight,
        ...sx,
      }}
      variant={variant}
      {...props}
    >
      {props.children}
    </MUIButton>
  );
};
