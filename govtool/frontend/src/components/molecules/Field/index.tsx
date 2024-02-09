import React, { PropsWithChildren } from "react";

import { Input } from "./Input";

type FieldComposition = React.FC<PropsWithChildren> & {
  Input: typeof Input;
};

const Field: FieldComposition = ({ children }) => {
  return <React.Fragment>{children}</React.Fragment>;
};

Field.Input = Input;

export { Field };

export * from "./types";
