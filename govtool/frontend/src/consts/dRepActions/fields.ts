import i18n from "@/i18n";
import { EMAIL_REGEX, NICKNAME_REGEX, URL_REGEX } from "@/utils";

export const Rules = {
  BIO: {
    maxLength: {
      value: 500,
      message: i18n.t("registration.fields.validations.maxLength", {
        maxLength: 500,
      }),
    },
  },
  DREP_NAME: {
    required: {
      value: true,
      message: i18n.t("registration.fields.validations.required"),
    },
    pattern: {
      value: NICKNAME_REGEX,
      message: i18n.t("registration.fields.validations.nickname"),
    },
    maxLength: {
      value: 80,
      message: i18n.t("registration.fields.validations.maxLength", {
        maxLength: 80,
      }),
    },
  },
  EMAIL: {
    pattern: {
      value: EMAIL_REGEX,
      message: i18n.t("registration.fields.validations.email"),
    },
  },
  LINK: {
    pattern: {
      value: URL_REGEX,
      message: i18n.t("registration.fields.validations.url"),
    },
  },
};
