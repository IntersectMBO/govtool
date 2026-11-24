import { CircularProgress, Button as MUIButton } from "@mui/material";
import { ButtonProps } from ".";

const HEIGHT = { extraLarge: 48, large: 40, medium: 36, small: 32 };
// Horizontal padding in theme spacing units (8px).
const PADDING_X = { extraLarge: 3, large: 2, medium: 1.75, small: 1.5 };

export const Button = ({
  size = "large",
  variant = "contained",
  sx,
  isLoading,
  dataTestId,
  ...props
}: ButtonProps) => (
  <MUIButton
    data-testid={dataTestId}
    sx={{
      fontSize: size === "extraLarge" ? 16 : 14,
      height: HEIGHT[size],
      px: PADDING_X[size],
      whiteSpace: "nowrap",
      ...(size === "extraLarge" && {
        letterSpacing: "-0.18px",
        lineHeight: "24px",
      }),
      ...sx,
    }}
    variant={variant}
    {...props}
    disabled={isLoading || props?.disabled}
  >
    {isLoading && <CircularProgress size={26} sx={{ position: "absolute" }} />}
    {props.children}
  </MUIButton>
);
