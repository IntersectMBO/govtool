import { InputFieldProps } from "@molecules";
import {
  Control,
  ControllerRenderProps,
  FieldErrors,
  FieldValues,
  Path,
  RegisterOptions,
} from "react-hook-form";

export type ControlledInputProps = InputFieldProps & {
  name: Path<any>;
  control: Control<any>;
  errors: FieldErrors<any>;
  rules?: Omit<RegisterOptions, "valueAsNumber" | "valueAsDate" | "setValueAs">;
};

export type RenderInputProps = {
  field: ControllerRenderProps<FieldValues, string>;
};
