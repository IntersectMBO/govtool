import { useCallback } from "react";
import { Controller, get } from "react-hook-form";

import { Field } from "@molecules";

import { ControlledCheckboxProps, RenderInputProps } from "./types";

export const Checkbox = ({
  control,
  name,
  errors,
  rules,
  ...props
}: ControlledCheckboxProps) => {
  const errorMessage = get(errors, name)?.message as string;

  const renderInput = useCallback(
    ({ field }: RenderInputProps) => (
      <Field.Checkbox
        checked={!!field.value}
        errorMessage={errorMessage}
        name={field.name}
        onChange={(newValue) => field.onChange(newValue)}
        value={field.value}
        {...props}
      />
    ),
    [errorMessage, props]
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
