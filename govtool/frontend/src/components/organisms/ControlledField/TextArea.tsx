import { useCallback } from "react";
import { Controller, get } from "react-hook-form";

import { Field } from "@molecules";

import { ControlledTextAreaProps, RenderInputProps } from "./types";

export function TextArea({
  control,
  name,
  errors,
  rules,
  ...props
}: ControlledTextAreaProps) {
  const errorMessage = get(errors, name)?.message as string;

  const renderInput = useCallback(
    ({ field }: RenderInputProps) => (
      <Field.TextArea {...props} {...field} errorMessage={errorMessage} />
    ),
    [errorMessage, props],
  );

  return (
    <Controller
      name={name}
      control={control}
      rules={rules}
      render={renderInput}
    />
  );
}
