import {
  CheckboxFieldProps,
  InputFieldProps,
  TextAreaFieldProps,
} from "@molecules";
import {
  Control,
  ControllerRenderProps,
  FieldErrors,
  FieldValues,
  Path,
  RegisterOptions,
} from "react-hook-form";

export type ControlledInputProps<
  TFieldValues extends FieldValues,
  TName extends Path<TFieldValues> = Path<TFieldValues>,
> =
  InputFieldProps & {
  control?: Control<TFieldValues>;
  errors?: FieldErrors<TFieldValues>;
  name: TName;
  rules?: Omit<
    RegisterOptions<TFieldValues, TName>,
    "valueAsNumber" | "valueAsDate" | "setValueAs"
  >;
};

export type ControlledCheckboxProps<
  TFieldValues extends FieldValues,
  TName extends Path<TFieldValues> = Path<TFieldValues>,
> = Omit<
  CheckboxFieldProps,
  "onChange" | "value"
> & {
  control: Control<TFieldValues>;
  errors: FieldErrors<TFieldValues>;
  name: TName;
  rules?: Omit<
    RegisterOptions<TFieldValues, TName>,
    "valueAsNumber" | "valueAsDate" | "setValueAs"
  >;
};

export type RenderInputProps<
  TFieldValues extends FieldValues,
  TName extends Path<TFieldValues>,
> = {
  field: ControllerRenderProps<TFieldValues, TName>;
};

export type ControlledTextAreaProps<
  TFieldValues extends FieldValues,
  TName extends Path<TFieldValues> = Path<TFieldValues>,
> =
  TextAreaFieldProps & {
  control: Control<TFieldValues>;
  errors: FieldErrors<TFieldValues>;
  name: TName;
  rules?: Omit<
    RegisterOptions<TFieldValues, TName>,
    "valueAsNumber" | "valueAsDate" | "setValueAs"
  >;
};
