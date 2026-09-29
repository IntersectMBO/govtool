import {
  forwardRef,
  useCallback,
  useId,
  useImperativeHandle,
  useRef,
} from "react";
import { InputBase } from "@mui/material";

import { InputProps } from "./types";

export const Input = forwardRef<HTMLInputElement, InputProps>(
  ({ errorMessage, dataTestId, onBlur, onFocus, sx, ...rest }, ref) => {
    const id = useId();
    const inputRef = useRef<HTMLInputElement>(null);

    const handleFocus = useCallback((e: React.FocusEvent<HTMLInputElement>) => {
      onFocus?.(e);
      inputRef.current?.focus();
    }, []);

    // onBlur is also forwarded to InputBase below. When the input has focus,
    // blurring it fires that DOM handler, so it is not called twice; otherwise
    // the imperative blur() still calls onBlur, as it always did.
    const handleBlur = useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        const element = inputRef.current;
        if (element && element === document.activeElement) {
          element.blur();
          return;
        }
        onBlur?.(e);
        element?.blur();
      },
      [onBlur],
    );

    useImperativeHandle(
      ref,
      () =>
        ({
          focus: handleFocus,
          blur: handleBlur,
          ...inputRef.current,
        } as unknown as HTMLInputElement),
      [handleBlur, handleFocus],
    );

    return (
      <InputBase
        id={id}
        inputProps={{ "data-testid": dataTestId }}
        inputRef={inputRef}
        onBlur={onBlur}
        sx={{
          backgroundColor: errorMessage ? "inputRed" : "white",
          border: 1,
          borderColor: errorMessage ? "red" : "secondaryBlue",
          borderRadius: 50,
          padding: "8px 16px",
          width: "100%",
          "& input.Mui-disabled": {
            WebkitTextFillColor: "#4C495B",
          },
          "&.Mui-disabled": {
            backgroundColor: "#F5F5F8",
            borderColor: "#9792B5",
          },
          ...sx,
        }}
        {...rest}
      />
    );
  },
);
