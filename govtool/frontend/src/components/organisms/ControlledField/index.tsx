import React, { PropsWithChildren } from "react";

import { Input } from "./Input";

type ControlledFieldComposition = React.FC<PropsWithChildren> & {
  Input: typeof Input;
};

const ControlledField: ControlledFieldComposition = ({ children }) => {
  return <>{children}</>;
};

ControlledField.Input = Input;

export { ControlledField };
