import {
  forwardRef,
  useCallback,
  useId,
  useImperativeHandle,
  useRef,
} from "react";
import { InputBase } from "@mui/material";

import { ds, RADIUS } from "@/consts/designTokens";

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
          backgroundColor: errorMessage ? ds.error["50"] : "#FFFFFF",
          border: 1,
          borderColor: errorMessage ? ds.error["500"] : ds.gray["200"],
          borderRadius: `${RADIUS.md}px`,
          color: ds.gray["900"],
          fontSize: 14,
          height: 38,
          letterSpacing: "-0.09px",
          lineHeight: "20px",
          padding: "8px 10px",
          width: "100%",
          "& input::placeholder": {
            color: ds.gray["500"],
            opacity: 1,
          },
          "&.Mui-focused": {
            borderColor: errorMessage ? ds.error["500"] : ds.primary["600"],
          },
          "& input.Mui-disabled": {
            WebkitTextFillColor: ds.gray["400"],
          },
          "&.Mui-disabled": {
            backgroundColor: ds.gray["50"],
            borderColor: ds.gray["200"],
          },
          ...sx,
        }}
        {...rest}
      />
    );
  },
);
