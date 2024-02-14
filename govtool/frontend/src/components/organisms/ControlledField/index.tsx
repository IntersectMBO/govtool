import React, { PropsWithChildren } from "react";

import { Checkbox } from "./Checkbox";
import { Input } from "./Input";

type ControlledFieldComposition = React.FC<PropsWithChildren> & {
  Checkbox: typeof Checkbox;
  Input: typeof Input;
};

const ControlledField: ControlledFieldComposition = ({ children }) => {
  return <>{children}</>;
};

ControlledField.Checkbox = Checkbox;
ControlledField.Input = Input;

export { ControlledField };
