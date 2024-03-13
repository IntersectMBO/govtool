import I18n from "@/i18n";
import {
  GovernanceActionType,
  GovernanceActionFields,
  GovernanceActionField,
  SharedGovernanceActionFieldSchema,
} from "@/types/governanceAction";
import { bech32 } from "bech32";

export const CIP_100 =
  "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#";
export const CIP_108 =
  "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#";

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

const commonContext = {
  "@language": "en-us",
  CIP100:
    "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#",
  CIP108:
    "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#",
  hashAlgorithm: "CIP100:hashAlgorithm",
  body: {
    "@id": "CIP108:body",
    "@context": {
      references: {
        "@id": "CIP108:references",
        "@container": "@set" as "@set",
        "@context": {
          GovernanceMetadata: "CIP100:GovernanceMetadataReference",
          Other: "CIP100:OtherReference",
          label: "CIP100:reference-label",
          uri: "CIP100:reference-uri",
          referenceHash: {
            "@id": "CIP108:referenceHash",
            "@context": {
              hashDigest: "CIP108:hashDigest",
              hashAlgorithm: "CIP100:hashAlgorithm",
            },
          },
        },
      },
      title: "CIP108:title",
      abstract: "CIP108:abstract",
      motivation: "CIP108:motivation",
      rationale: "CIP108:rationale",
    },
  },
  authors: {
    "@id": "CIP100:authors",
    "@container": "@set" as "@set",
    "@context": {
      name: "http://xmlns.com/foaf/0.1/name",
      witness: {
        "@id": "CIP100:witness",
        "@context": {
          witnessAlgorithm: "CIP100:witnessAlgorithm",
          publicKey: "CIP100:publicKey",
          signature: "CIP100:signature",
        },
      },
    },
  },
};

export const GOVERNANCE_ACTION_CONTEXTS = {
  [GovernanceActionType.Info]: commonContext,
  [GovernanceActionType.Treasury]: {
    ...commonContext,
    body: {
      ...commonContext.body,
      "@context": {
        ...commonContext.body["@context"],
        amount: "CIP108:amount",
        receivingAddress: "CIP108:receivingAddress",
      },
    },
  },
};
