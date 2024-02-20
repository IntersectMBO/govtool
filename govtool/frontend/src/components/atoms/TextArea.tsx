import { TextareaAutosize } from "@mui/material";

import { useScreenDimension } from "@hooks";

import { TextAreaProps } from "./types";

export const TextArea = ({
  errorMessage,
  maxLength = 500,
  ...props
}: TextAreaProps) => {
  const { isMobile } = useScreenDimension();

  return (
    <TextareaAutosize
      style={{
        border: `1px solid ${errorMessage ? "red" : "#6F99FF"}`,
        borderRadius: "24px",
        height: isMobile ? "104px" : "128px",
        // 600 - paddingHorizontal
        maxWidth: "572px",
        padding: "12px 14px",
        resize: "none",
        width: "100%",
      }}
      maxRows={3}
      maxLength={maxLength}
      {...props}
    />
  );
};
