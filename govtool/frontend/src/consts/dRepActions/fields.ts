import i18n from "@/i18n";
import { URL_REGEX, isReceivingAddress, isValidURLLength } from "@/utils";

export const Rules = {
  GIVEN_NAME: {
    required: {
      value: true,
      message: i18n.t("registration.fields.validations.required"),
    },
    maxLength: {
      value: 80,
      message: i18n.t("registration.fields.validations.maxLength", {
        maxLength: 80,
      }),
    },
    pattern: {
      value: /^[^\s]+$/,
      message: i18n.t("registration.fields.validations.noSpaces"),
    },
  },
  LINK: {
    pattern: {
      value: URL_REGEX,
      message: i18n.t("registration.fields.validations.url"),
    },
  },
  STORING_LINK: {
    required: {
      value: true,
      message: i18n.t("registration.fields.validations.required"),
    },
    pattern: {
      value: URL_REGEX,
      message: i18n.t("registration.fields.validations.url"),
    },
    validate: isValidURLLength,
  },
  MOTIVATIONS: {
    maxLength: {
      value: 1000,
      message: i18n.t("registration.fields.validations.maxLength", {
        maxLength: 1000,
      }),
    },
  },
  OBJECTIVES: {
    maxLength: {
      value: 1000,
      message: i18n.t("registration.fields.validations.maxLength", {
        maxLength: 1000,
      }),
    },
  },
  PAYMENT_ADDRESS: {
    validate: isReceivingAddress,
  },
  QUALIFICATIONS: {
    maxLength: {
      value: 1000,
      message: i18n.t("registration.fields.validations.maxLength", {
        maxLength: 1000,
      }),
    },
  },
};
