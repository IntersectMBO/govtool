import { useTranslation } from "react-i18next";
import { TranslationKey } from "./types";

interface InterpolationValues {
  [key: string]: number | string | undefined;
}

export const usei18n = () => {
  const { t: i18nT } = useTranslation();

  const t = (key: TranslationKey, interpolation?: InterpolationValues) =>
    i18nT(key, interpolation);

  return { t };
};
