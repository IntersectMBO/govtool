import { forwardRef, useCallback, useImperativeHandle, useRef } from "react";
import { Box } from "@mui/material";

import { FormErrorMessage, Input as InputBase, Typography } from "@atoms";

import { InputFieldProps } from "./types";

export const Input = forwardRef<HTMLInputElement, InputFieldProps>(
  (
    {
      errorMessage,
      errorStyles,
      label,
      labelStyles,
      layoutStyles,
      onBlur,
      onFocus,
      ...rest
    },
    ref
  ) => {
    const inputRef = useRef<HTMLInputElement>(null);

    const handleFocus = useCallback((e: React.FocusEvent<HTMLInputElement>) => {
      onFocus?.(e);
      inputRef.current?.focus();
    }, []);

    const handleBlur = useCallback((e: React.FocusEvent<HTMLInputElement>) => {
      onBlur?.(e);
      inputRef.current?.blur();
    }, []);

    useImperativeHandle(
      ref,
      () =>
        ({
          focus: handleFocus,
          blur: handleBlur,
          ...inputRef.current,
        } as unknown as HTMLInputElement),
      [handleBlur, handleFocus]
    );

    return (
      <Box sx={{ width: "100%", ...layoutStyles }}>
        {label && (
          <Typography fontWeight={400} variant="body2" {...labelStyles}>
            {label}
          </Typography>
        )}
        <InputBase errorMessage={errorMessage} {...rest} ref={inputRef} />
        <FormErrorMessage
          errorMessage={errorMessage}
          errorStyles={errorStyles}
        />
      </Box>
    );
  }
);
