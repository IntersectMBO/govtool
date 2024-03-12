import I18n from "@/i18n";
import {
  GovernanceActionType,
  GovernanceActionFields,
  GovernanceActionField,
  SharedGovernanceActionFieldSchema,
} from "@/types/governanceAction";
import { bech32 } from "bech32";

const sharedGovernanceActionFields: SharedGovernanceActionFieldSchema = {
  title: {
    component: GovernanceActionField.Input,
    labelI18nKey: "createGovernanceAction.fields.declarations.title.label",
    placeholderI18nKey:
      "createGovernanceAction.fields.declarations.title.placeholder",
    rules: {
      required: {
        value: true,
        message: I18n.t("createGovernanceAction.fields.validations.required"),
      },
    },
  },
  abstract: {
    component: GovernanceActionField.TextArea,
    labelI18nKey: "createGovernanceAction.fields.declarations.abstract.label",
    placeholderI18nKey:
      "createGovernanceAction.fields.declarations.abstract.placeholder",
    tipI18nKey: "createGovernanceAction.fields.declarations.abstract.tip",
    rules: {
      required: {
        value: true,
        message: I18n.t("createGovernanceAction.fields.validations.required"),
      },
      maxLength: {
        value: 500,
        message: I18n.t("createGovernanceAction.fields.validations.maxLength", {
          maxLength: 500,
        }),
      },
    },
  },
  motivation: {
    component: GovernanceActionField.TextArea,
    labelI18nKey: "createGovernanceAction.fields.declarations.motivation.label",
    placeholderI18nKey:
      "createGovernanceAction.fields.declarations.motivation.placeholder",
    tipI18nKey: "createGovernanceAction.fields.declarations.motivation.tip",
    rules: {
      required: {
        value: true,
        message: I18n.t("createGovernanceAction.fields.validations.required"),
      },
      maxLength: {
        value: 500,
        message: I18n.t("createGovernanceAction.fields.validations.maxLength", {
          maxLength: 500,
        }),
      },
    },
  },
  rationale: {
    component: GovernanceActionField.TextArea,
    labelI18nKey: "createGovernanceAction.fields.declarations.rationale.label",
    placeholderI18nKey:
      "createGovernanceAction.fields.declarations.rationale.placeholder",
    tipI18nKey: "createGovernanceAction.fields.declarations.rationale.tip",
    rules: {
      required: {
        value: true,
        message: I18n.t("createGovernanceAction.fields.validations.required"),
      },
      maxLength: {
        value: 500,
        message: I18n.t("createGovernanceAction.fields.validations.maxLength", {
          maxLength: 500,
        }),
      },
    },
  },
};

export const GOVERNANCE_ACTION_FIELDS: GovernanceActionFields = {
  [GovernanceActionType.Info]: sharedGovernanceActionFields,
  [GovernanceActionType.Treasury]: {
    ...sharedGovernanceActionFields,
    receivingAddress: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.receivingAddress.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.receivingAddress.placeholder",
      rules: {
        validate: (value) => {
          if (bech32.decode(value).words.length) {
            return true;
          } else {
            return I18n.t("createGovernanceAction.fields.validations.bech32");
          }
        },
      },
    },
    amount: {
      component: GovernanceActionField.Input,
      labelI18nKey: "createGovernanceAction.fields.declarations.amount.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.amount.placeholder",
      rules: {
        required: {
          value: true,
          message: I18n.t("createGovernanceAction.fields.validations.required"),
        },
        validate: (value) => {
          if (Number.isInteger(Number(value))) {
            return true;
          } else {
            return I18n.t("createGovernanceAction.fields.validations.number");
          }
        },
      },
    },
  },
} as const;
