import { Button as MUIButton } from '@mui/material';
import { ButtonProps } from '.';

export const Button = ({
  size = 'large',
  variant = 'contained',
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
        fontSize: size === 'extraLarge' ? 16 : 14,
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
