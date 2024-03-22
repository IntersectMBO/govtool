import { useMemo } from "react";
import { yupResolver } from "@hookform/resolvers/yup";
import { useForm } from "react-hook-form";
import * as Yup from "yup";
import { HASH_REGEX, URL_REGEX } from "@utils";
import { useTranslation } from "@hooks";

export interface UrlAndHashFormValues {
  hash?: string;
  storeData?: boolean;
  url?: string;
}

export const useUrlAndHashFormController = () => {
  const { t } = useTranslation();

  const validationSchema = useMemo(
    () =>
      Yup.object().shape({
        url: Yup.string()
          .trim()
          .max(64, t("forms.errors.urlTooLong"))
          .test(
            "url-validation",
            t("forms.errors.urlInvalidFormat"),
            (value) => !value || URL_REGEX.test(value),
          ),
        hash: Yup.string()
          .trim()
          .test(
            "hash-length-validation",
            t("forms.errors.hashInvalidLength"),
            (value) => !value || value.length === 64,
          )
          .test(
            "hash-format-validation",
            t("forms.errors.hashInvalidFormat"),
            (value) => !value || HASH_REGEX.test(value),
          ),
        storeData: Yup.boolean(),
      }),
    [],
  );

  return useForm<UrlAndHashFormValues>({
    defaultValues: { url: "", hash: "", storeData: false },
    mode: "onChange",
    resolver: yupResolver<UrlAndHashFormValues>(validationSchema),
  });
};
