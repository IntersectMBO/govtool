import { useMemo } from "react";
import { yupResolver } from "@hookform/resolvers/yup";
import { useForm } from "react-hook-form";
import * as Yup from "yup";
import { HASH_REGEX, URL_REGEX } from "@utils";

export interface UrlAndHashFormValues {
  url?: string;
  hash?: string;
}

export const useUrlAndHashFormController = () => {
  const validationSchema = useMemo(
    () =>
      Yup.object().shape({
        url: Yup.string()
          .trim()
          .max(64, "Url must be less than 65 characters")
          .test("url-validation", "Invalid URL format", (value) => {
            return !value || URL_REGEX.test(value);
          }),
        hash: Yup.string()
          .trim()
          .test(
            "hash-length-validation",
            "Hash must be exactly 64 characters long",
            (value) => {
              return !value || value.length === 64;
            }
          )
          .test("hash-format-validation", "Invalid hash format", (value) => {
            return !value || HASH_REGEX.test(value);
          }),
      }),
    []
  );

  return useForm<UrlAndHashFormValues>({
    defaultValues: { url: "", hash: "" },
    mode: "onChange",
    resolver: yupResolver<UrlAndHashFormValues>(validationSchema),
  });
};
