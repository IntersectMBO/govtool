import { useCallback } from "react";
import { Controller, FieldValues, get, Path } from "react-hook-form";

import { Field } from "@molecules";

import { ControlledTextAreaProps, RenderInputProps } from "./types";

export const TextArea = <
  TFieldValues extends FieldValues,
  TName extends Path<TFieldValues>,
>({
  control,
  name,
  errors,
  rules,
  ...props
}: ControlledTextAreaProps<TFieldValues, TName>) => {
  const errorMessage = get(errors, name)?.message as string;

  const renderInput = useCallback(
    ({ field }: RenderInputProps<TFieldValues, TName>) => (
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
};
