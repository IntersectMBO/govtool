import { isRewardAddress, numberValidation } from "@utils";
import I18n from "@/i18n";
import {
  GovernanceActionType,
  GovernanceActionFields,
  GovernanceActionField,
  SharedGovernanceActionFieldSchema,
} from "@/types/governanceAction";

export const GovernanceActionTooltip = {
  InfoAction: I18n.t("govActions.tooltips.info"),
  TreasuryWithdrawals: I18n.t("govActions.tooltips.treasury"),
};

export const CIP_100 =
  "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#";
export const CIP_108 =
  "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#";

export const sharedGovernanceActionFields: SharedGovernanceActionFieldSchema = {
  title: {
    component: GovernanceActionField.Input,
    labelI18nKey: "createGovernanceAction.fields.declarations.title.label",
    placeholderI18nKey:
      "createGovernanceAction.fields.declarations.title.placeholder",
    rules: {
      maxLength: {
        value: 80,
        message: I18n.t("createGovernanceAction.fields.validations.maxLength", {
          maxLength: 80,
        }),
      },
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
  [GovernanceActionType.InfoAction]: sharedGovernanceActionFields,
  [GovernanceActionType.NoConfidence]: sharedGovernanceActionFields,
  [GovernanceActionType.NewCommittee]: {
    ...sharedGovernanceActionFields,
    numerator: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.numerator.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.numerator.placeholder",
      rules: {
        required: {
          value: true,
          message: I18n.t("createGovernanceAction.fields.validations.required"),
        },
        validate: numberValidation,
      },
    },
    denominator: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.denominator.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.denominator.placeholder",
      rules: {
        required: {
          value: true,
          message: I18n.t("createGovernanceAction.fields.validations.required"),
        },
        validate: numberValidation,
      },
    },
    newCommitteeHash: {
      component: GovernanceActionField.Input,
      labelI18nKey: "createGovernanceAction.fields.declarations.members.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.members.placeholder",
      tipI18nKey: "createGovernanceAction.fields.declarations.members.tip",
      rules: {
        required: {
          value: true,
          message: I18n.t("createGovernanceAction.fields.validations.required"),
        },
        maxLength: {
          value: 500,
          message: I18n.t(
            "createGovernanceAction.fields.validations.maxLength",
            {
              maxLength: 500,
            },
          ),
        },
      },
    },
    newCommitteeExpiryEpoch: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.expiryEpoch.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.expiryEpoch.placeholder",
      rules: {
        required: {
          value: true,
          message: I18n.t("createGovernanceAction.fields.validations.required"),
        },
        validate: numberValidation,
      },
    },
    removeCommitteeHash: {
      component: GovernanceActionField.Input,
      labelI18nKey: "createGovernanceAction.fields.declarations.remove.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.remove.placeholder",
      tipI18nKey: "createGovernanceAction.fields.declarations.remove.tip",
      rules: {
        maxLength: {
          value: 500,
          message: I18n.t(
            "createGovernanceAction.fields.validations.maxLength",
            {
              maxLength: 500,
            },
          ),
        },
      },
    },
  },
  [GovernanceActionType.TreasuryWithdrawals]: {
    ...sharedGovernanceActionFields,
    receivingAddress: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.receivingAddress.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.receivingAddress.placeholder",
      rules: {
        validate: isRewardAddress,
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
        validate: numberValidation,
      },
    },
  },
  [GovernanceActionType.NewConstitution]: {
    ...sharedGovernanceActionFields,
    prevGovernanceActionHash: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.prevGovernanceActionHash.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.prevGovernanceActionHash.placeholder",
    },
    prevGovernanceActionIndex: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.prevGovernanceActionIndex.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.prevGovernanceActionIndex.placeholder",
      rules: {
        validate: numberValidation,
      },
    },
    constitutionUrl: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.constitutionUrl.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.constitutionUrl.placeholder",
      rules: {
        required: {
          value: true,
          message: I18n.t("createGovernanceAction.fields.validations.required"),
        },
      },
    },
    constitutionHash: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.constitutionHash.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.constitutionHash.placeholder",
      rules: {
        required: {
          value: true,
          message: I18n.t("createGovernanceAction.fields.validations.required"),
        },
      },
    },
    scriptHash: {
      component: GovernanceActionField.Input,
      labelI18nKey:
        "createGovernanceAction.fields.declarations.scriptHash.label",
      placeholderI18nKey:
        "createGovernanceAction.fields.declarations.scriptHash.placeholder",
    },
  },
} as const;

export const GOVERNANCE_ACTION_CONTEXT = {
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
        "@container": "@set" as const,
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
    "@container": "@set" as const,
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
