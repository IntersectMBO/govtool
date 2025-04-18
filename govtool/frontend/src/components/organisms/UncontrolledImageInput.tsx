/* eslint-disable @typescript-eslint/no-explicit-any */
import { useRef } from "react";
import { useController } from "react-hook-form";
import { FormErrorMessage } from "../atoms";

type UncontrolledImageInputProps = {
  name: string;
  control: any;
  rules?: any;
  placeholder?: string;
  dataTestId?: string;
};

export const UncontrolledImageInput = ({
  name,
  control,
  rules,
  placeholder,
  dataTestId,
}: UncontrolledImageInputProps) => {
  const {
    field: { onChange },
    fieldState,
  } = useController({
    name,
    control,
    rules,
  });

  const inputRef = useRef<HTMLInputElement>(null);

  return (
    <div style={{ width: "100%" }}>
      <input
        ref={inputRef}
        onBlur={() => {
          const value = inputRef.current?.value;
          if (value !== undefined) {
            onChange(value);
          }
        }}
        placeholder={placeholder}
        data-testid={dataTestId}
        defaultValue=""
        style={{
          width: "100%",
          fontSize: "inherit",
          padding: "8px 16px",
          borderRadius: "50px",
          height: "50px",
          border: "1px solid",
          borderColor: fieldState.error?.message ? "red" : "#6F99FF",
          backgroundColor: fieldState.error?.message ? "#FAEAEB" : "white",
          boxSizing: "border-box",
          margin: 0,
          display: "block",
          minWidth: 0,
          lineHeight: "1.4375em",
          WebkitTapHighlightColor: "transparent",
          animationDuration: "10ms",
          WebkitAnimationDuration: "10ms",
          animationName: "mui-auto-fill-cancel",
          WebkitAnimationName: "mui-auto-fill-cancel",
          outline: "none",
        }}
      />
      {fieldState.error && (
        <FormErrorMessage
          dataTestId={`${dataTestId}-error`}
          errorMessage={fieldState.error.message}
        />
      )}
    </div>
  );
};
