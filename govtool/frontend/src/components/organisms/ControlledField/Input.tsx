import { forwardRef, useCallback } from "react";
import { Controller, get } from "react-hook-form";

import { Field } from "@molecules";

import { ControlledInputProps, RenderInputProps } from "./types";

export const Input = forwardRef<HTMLInputElement, ControlledInputProps>(
  ({
    control, name, errors, rules, ...props
  }, ref) => {
    const errorMessage = get(errors, name)?.message as string;

    const renderInput = useCallback(
      ({ field }: RenderInputProps) => (
        <Field.Input
          {...props}
          {...field}
          errorMessage={errorMessage}
          ref={ref}
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
  },
);
