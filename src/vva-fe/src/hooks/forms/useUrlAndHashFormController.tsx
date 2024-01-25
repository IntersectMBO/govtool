import { useMemo } from "react";
import { yupResolver } from "@hookform/resolvers/yup";
import { useForm } from "react-hook-form";
import * as Yup from "yup";
import { HASH_REGEX, URL_REGEX } from "@utils";
import { usei18n } from "@translations";

export interface UrlAndHashFormValues {
  url?: string;
  hash?: string;
}

export const useUrlAndHashFormController = () => {
  const { t } = usei18n();

  const validationSchema = useMemo(
    () =>
      Yup.object().shape({
        url: Yup.string()
          .trim()
          .max(64, t("forms.errors.urlTooLong"))
          .test(
            "url-validation",
            t("forms.errors.urlInvalidFormat"),
            (value) => {
              return !value || URL_REGEX.test(value);
            }
          ),
        hash: Yup.string()
          .trim()
          .test(
            "hash-length-validation",
            t("forms.errors.hashInvalidLength"),
            (value) => {
              return !value || value.length === 64;
            }
          )
          .test(
            "hash-format-validation",
            t("forms.errors.hashInvalidFormat"),
            (value) => {
              return !value || HASH_REGEX.test(value);
            }
          ),
      }),
    []
  );

  return useForm<UrlAndHashFormValues>({
    defaultValues: { url: "", hash: "" },
    mode: "onChange",
    resolver: yupResolver<UrlAndHashFormValues>(validationSchema),
  });
};
