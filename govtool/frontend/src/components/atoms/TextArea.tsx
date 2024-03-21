import { forwardRef, useCallback, useImperativeHandle, useRef } from "react";
import { TextareaAutosize, styled } from "@mui/material";

import { useScreenDimension } from "@hooks";

import { TextAreaProps } from "./types";

const TextAreaBase = styled(TextareaAutosize)(
  () => `
  font-family: "Poppins";
  font-size: 16px;
  font-weight: 400;
    ::placeholder {
      font-family: "Poppins";
      font-size: 16px;
      font-weight: 400;
      color: #a6a6a6;
    }
    `,
);

export const TextArea = forwardRef<HTMLTextAreaElement, TextAreaProps>(
  ({ errorMessage, maxLength = 500, onBlur, onFocus, ...props }, ref) => {
    const { isMobile } = useScreenDimension();
    const textAraeRef = useRef<HTMLTextAreaElement>(null);

    const handleFocus = useCallback(
      (e: React.FocusEvent<HTMLTextAreaElement>) => {
        onFocus?.(e);
        textAraeRef.current?.focus();
      },
      [],
    );

    const handleBlur = useCallback(
      (e: React.FocusEvent<HTMLTextAreaElement>) => {
        onBlur?.(e);
        textAraeRef.current?.blur();
      },
      [],
    );

    useImperativeHandle(
      ref,
      () =>
        ({
          focus: handleFocus,
          blur: handleBlur,
          ...textAraeRef.current,
        } as unknown as HTMLTextAreaElement),
      [handleBlur, handleFocus],
    );

    return (
      <TextAreaBase
        style={{
          border: `1px solid ${errorMessage ? "red" : "#6F99FF"}`,
          backgroundColor: errorMessage ? "#FAEAEB" : "white",
          borderRadius: "24px",
          height: isMobile ? "104px" : "128px",
          outline: "none",
          padding: "12px 14px",
          resize: "none",
        }}
        maxLength={maxLength}
        ref={textAraeRef}
        {...props}
      />
    );
  },
);
