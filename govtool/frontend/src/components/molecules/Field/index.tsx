import React, { PropsWithChildren } from "react";

import { Checkbox } from "./Checkbox";
import { Input } from "./Input";

type FieldComposition = React.FC<PropsWithChildren> & {
  Input: typeof Input;
  Checkbox: typeof Checkbox;
};

const Field: FieldComposition = ({ children }) => {
  return <React.Fragment>{children}</React.Fragment>;
};

Field.Checkbox = Checkbox;
Field.Input = Input;

export { Field };

export * from "./types";
