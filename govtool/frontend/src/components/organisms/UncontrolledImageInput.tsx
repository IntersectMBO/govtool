/* eslint-disable @typescript-eslint/no-explicit-any */
import { useRef } from "react";
import {
  Control,
  FieldPath,
  FieldValues,
  RegisterOptions,
  useController,
} from "react-hook-form";
import { FormErrorMessage } from "../atoms";

type UncontrolledImageInputProps<
  TFieldValues extends FieldValues = FieldValues,
  TName extends FieldPath<TFieldValues> = FieldPath<TFieldValues>,
> = {
  name: TName;
  control: Control<TFieldValues>;
  rules?: RegisterOptions<TFieldValues, TName>;
  placeholder?: string;
  dataTestId?: string;
};

export const UncontrolledImageInput = <T extends FieldValues>({
  name,
  control,
  rules,
  placeholder,
  dataTestId,
}: UncontrolledImageInputProps<T>) => {
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
          borderColor: fieldState.error ? "red" : "#6F99FF",
          backgroundColor: fieldState.error ? "#FAEAEB" : "white",
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
