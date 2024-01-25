import React, { ReactElement } from "react";
import { Trans } from "react-i18next";
import { TranslationKey } from "./types";

interface TypedTransProps {
  i18nKey: TranslationKey;
  values?: Record<string, number | string | undefined>;
  components?: ReactElement[];
}

export const TypedTrans: React.FC<TypedTransProps> = ({
  i18nKey,
  values,
  components,
}) => {
  return <Trans i18nKey={i18nKey} values={values} components={components} />;
};
