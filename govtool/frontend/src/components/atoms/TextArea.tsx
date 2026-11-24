import { forwardRef, useCallback, useImperativeHandle, useRef } from "react";
import { TextareaAutosize, styled } from "@mui/material";

import { useScreenDimension } from "@hooks";
import { ds, FONT_FAMILY, RADIUS } from "@/consts/designTokens";

import { TextAreaProps } from "./types";

const TextAreaBase = styled(TextareaAutosize)(
  () => `
  font-family: ${FONT_FAMILY.sans};
  font-weight: 400;
    ::placeholder {
      font-family: ${FONT_FAMILY.sans};
      font-weight: 400;
      color: ${ds.gray["500"]};
    }
    `,
);

export const TextArea = forwardRef<HTMLTextAreaElement, TextAreaProps>(
  (
    {
      errorMessage,
      maxLength = 500,
      onBlur,
      onFocus,
      isModifiedLayout,
      ...props
    },
    ref,
  ) => {
    const { isMobile } = useScreenDimension();
    const textAraeRef = useRef<HTMLTextAreaElement>(null);

    const handleFocus = useCallback(
      (e: React.FocusEvent<HTMLTextAreaElement>) => {
        onFocus?.(e);
        textAraeRef.current?.focus();
      },
      [],
    );

    // onBlur is also forwarded to the <textarea> below. When the textarea has
    // focus, blurring it fires that DOM handler, so it is not called twice;
    // otherwise the imperative blur() still calls onBlur, as it always did.
    const handleBlur = useCallback(
      (e: React.FocusEvent<HTMLTextAreaElement>) => {
        const element = textAraeRef.current;
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
          ...textAraeRef.current,
        } as unknown as HTMLTextAreaElement),
      [handleBlur, handleFocus],
    );

    const getTexAreaHeight = () => {
      if (isModifiedLayout && isMobile) return "312px";
      if (isModifiedLayout) return "208px";
      if (isMobile) return "104px";
      return "128px";
    };

    return (
      <TextAreaBase
        style={{
          border: `1px solid ${errorMessage ? ds.error["500"] : ds.gray["200"]}`,
          backgroundColor: errorMessage ? ds.error["50"] : "#FFFFFF",
          borderRadius: `${RADIUS.md}px`,
          height: getTexAreaHeight(),
          outline: "none",
          padding: "12px 14px",
          resize: "none",
        }}
        maxLength={maxLength}
        onBlur={onBlur}
        ref={textAraeRef}
        sx={{
          fontSize: isModifiedLayout ? "12px" : "auto",
          "&::placeholder": {
            fontSize: isModifiedLayout ? "12px" : "16px",
          },
        }}
        {...props}
      />
    );
  },
);
