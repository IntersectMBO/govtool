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

export type ControlledInputProps = InputFieldProps & {
  control: Control<any>;
  errors: FieldErrors<any>;
  name: Path<any>;
  rules?: Omit<RegisterOptions, "valueAsNumber" | "valueAsDate" | "setValueAs">;
};

export type ControlledCheckboxProps = Omit<
  CheckboxFieldProps,
  "onChange" | "value"
> & {
  control: Control<any>;
  errors: FieldErrors<any>;
  name: Path<any>;
  rules?: Omit<RegisterOptions, "valueAsNumber" | "valueAsDate" | "setValueAs">;
};

export type RenderInputProps = {
  field: ControllerRenderProps<FieldValues, string>;
};

export type ControlledTextAreaProps = TextAreaFieldProps & {
  control: Control<any>;
  errors: FieldErrors<any>;
  name: Path<any>;
  rules?: Omit<RegisterOptions, "valueAsNumber" | "valueAsDate" | "setValueAs">;
};
