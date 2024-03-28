import { Box } from "@mui/material";

import {
  FormErrorMessage,
  FormHelpfulText,
  TextArea as TextAreaBase,
  Typography,
} from "@atoms";

import { forwardRef, useCallback, useImperativeHandle, useRef } from "react";
import { TextAreaFieldProps } from "./types";

export const TextArea = forwardRef<HTMLTextAreaElement, TextAreaFieldProps>(
  (
    {
      errorMessage,
      errorStyles,
      helpfulText,
      helpfulTextStyle,
      label,
      labelStyles,
      layoutStyles,
      maxLength = 500,
      onBlur,
      onFocus,
      ...props
    },
    ref,
  ) => {
    const textAreaRef = useRef<HTMLTextAreaElement>(null);

    const handleFocus = useCallback(
      (e: React.FocusEvent<HTMLTextAreaElement>) => {
        onFocus?.(e);
        textAreaRef.current?.focus();
      },
      [],
    );

    const handleBlur = useCallback(
      (e: React.FocusEvent<HTMLTextAreaElement>) => {
        onBlur?.(e);
        textAreaRef.current?.blur();
      },
      [],
    );

    useImperativeHandle(
      ref,
      () =>
        ({
          focus: handleFocus,
          blur: handleBlur,
          ...textAreaRef.current,
        } as unknown as HTMLTextAreaElement),
      [handleBlur, handleFocus],
    );

    const getCounterBottomSxValue = () => {
      if (props.isModifiedLayout && errorMessage) return 30;
      if (props.isModifiedLayout) return 10;
      if (errorMessage) return 52.5;
      return 35;
    };

    return (
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          width: "100%",
          position: "relative",
          ...layoutStyles,
        }}
      >
        {label && (
          <Typography
            fontWeight={400}
            sx={{ mb: 0.5 }}
            variant="body2"
            {...labelStyles}
          >
            {label}
          </Typography>
        )}
        <TextAreaBase
          errorMessage={errorMessage}
          maxLength={maxLength}
          {...props}
          ref={textAreaRef}
        />
        <FormHelpfulText
          helpfulText={helpfulText}
          helpfulTextStyle={helpfulTextStyle}
        />
        <FormErrorMessage
          errorMessage={errorMessage}
          errorStyles={errorStyles}
        />
        <Typography
          color="#8E908E"
          sx={{
            bottom: getCounterBottomSxValue(),
            position: "absolute",
            right: 15,
          }}
          variant="caption"
        >
          {props?.value?.toString()?.length ?? 0}/{maxLength}
        </Typography>
      </Box>
    );
  },
);
