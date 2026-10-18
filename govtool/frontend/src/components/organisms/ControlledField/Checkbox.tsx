import { useCallback } from "react";
import { Controller, FieldValues, get, Path } from "react-hook-form";

import { Field } from "@molecules";

import { ControlledCheckboxProps, RenderInputProps } from "./types";

export const Checkbox = <
  TFieldValues extends FieldValues,
  TName extends Path<TFieldValues>,
>({
  control,
  name,
  errors,
  rules,
  ...props
}: ControlledCheckboxProps<TFieldValues, TName>) => {
  const errorMessage = get(errors, name)?.message as string;

  const renderInput = useCallback(
    ({ field }: RenderInputProps<TFieldValues, TName>) => (
      <Field.Checkbox
        checked={!!field.value}
        errorMessage={errorMessage}
        name={field.name}
        onChange={field.onChange}
        value={field.value}
        {...props}
      />
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
