import i18n from "@/i18n";
import {
  PAYMENT_ADDRESS_REGEX,
  URL_REGEX,
  isValidURLLength,
} from "@/utils";

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
    pattern: {
      value: PAYMENT_ADDRESS_REGEX,
      message: i18n.t("registration.fields.validations.paymentAddress"),
    }
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
