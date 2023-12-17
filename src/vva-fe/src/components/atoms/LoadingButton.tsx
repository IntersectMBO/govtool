import { Button, CircularProgress } from '@mui/material';
import type { ButtonProps } from '@mui/material';

export type ButtonIntent = 'primary' | 'secondary';
export interface ExtendedButtonProps extends Omit<ButtonProps, 'size'> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any -- conflicting event types for form and button event handlers
  onClick?: (e?: any) => void;
}

interface Props extends ExtendedButtonProps {
  isLoading: boolean;
}

export const LoadingButton = ({ isLoading, disabled, children, ...rest }: Props) => {
  return (
    <Button disabled={disabled || isLoading} {...rest}>
      {isLoading ? (
        <CircularProgress />
      ) : (
        children
      )}
    </Button>
  );
}
