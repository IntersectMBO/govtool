import { TextareaAutosize, styled } from "@mui/material";

import { useScreenDimension } from "@hooks";

import { TextAreaProps } from "./types";

const TextAreaBase = styled(TextareaAutosize)(
  () => `
    ::placeholder {
      font-family: "Poppins";
      font-size: 16px;
      font-weight: 400;
      color: #a6a6a6;
    }
    `
);

export const TextArea = ({
  errorMessage,
  maxLength = 500,
  ...props
}: TextAreaProps) => {
  const { isMobile } = useScreenDimension();

  return (
    <TextAreaBase
      style={{
        border: `1px solid ${errorMessage ? "red" : "#6F99FF"}`,
        borderRadius: "24px",
        height: isMobile ? "104px" : "128px",
        outline: "none",
        padding: "12px 14px",
        resize: "none",
      }}
      maxLength={maxLength}
      {...props}
    />
  );
};
