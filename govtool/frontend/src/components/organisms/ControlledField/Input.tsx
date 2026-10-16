import { forwardRef, ReactElement, Ref, useCallback } from "react";
import { Controller, FieldValues, get } from "react-hook-form";

import { Field } from "@molecules";

import { ControlledInputProps, RenderInputProps } from "./types";

const InputInner = <
  TFieldValues extends FieldValues,
  TName extends import("react-hook-form").Path<TFieldValues>,
>(
  { control, name, errors, rules, ...props }: ControlledInputProps<TFieldValues, TName>,
  ref: Ref<HTMLInputElement>,
) => {
    const errorMessage = get(errors, name)?.message as string;
    const renderInput = useCallback(
      ({ field }: RenderInputProps<TFieldValues, TName>) => (
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
};

export const Input = forwardRef(InputInner) as <
  TFieldValues extends FieldValues,
  TName extends import("react-hook-form").Path<TFieldValues>,
>(
  props: ControlledInputProps<TFieldValues, TName> & { ref?: Ref<HTMLInputElement> },
) => ReactElement;
