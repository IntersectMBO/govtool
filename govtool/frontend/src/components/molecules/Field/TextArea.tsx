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
      errorDataTestId,
      errorMessage,
      errorStyles,
      helpfulText,
      helpfulTextStyle,
      hideLabel,
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

    // The TextArea atom owns onBlur (it forwards it to the <textarea> and
    // calls it from its own imperative blur), so this only delegates.
    const handleBlur = useCallback(
      (e: React.FocusEvent<HTMLTextAreaElement>) => {
        (
          textAreaRef.current?.blur as
            | ((event?: React.FocusEvent<HTMLTextAreaElement>) => void)
            | undefined
        )?.(e);
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
            sx={{ mb: 0.5, ...(hideLabel && { fontSize: 0, lineHeight: 0 }) }}
            variant="body2"
            {...labelStyles}
          >
            {label}
          </Typography>
        )}
        <Box
          sx={{
            display: "flex",
            flexDirection: "column",
            position: "relative",
          }}
        >
          <TextAreaBase
            errorMessage={errorMessage}
            maxLength={maxLength}
            onBlur={onBlur}
            {...props}
            ref={textAreaRef}
          />
          <Typography
            color="#8E908E"
            sx={{
              bottom: 12,
              position: "absolute",
              right: 14,
            }}
            variant="caption"
          >
            {props?.value?.toString()?.length ?? 0}/{maxLength}
          </Typography>
        </Box>
        <FormHelpfulText
          helpfulText={helpfulText}
          helpfulTextStyle={helpfulTextStyle}
        />
        <FormErrorMessage
          dataTestId={errorDataTestId}
          errorMessage={errorMessage}
          errorStyles={errorStyles}
        />
      </Box>
    );
  },
);
