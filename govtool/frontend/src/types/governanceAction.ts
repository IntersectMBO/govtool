import { RegisterOptions } from "react-hook-form";
import en from "@/i18n/locales/en.json";

export enum GovernanceActionType {
  ParameterChange = "ParameterChange",
  HardForkInitiation = "HardForkInitiation",
  TreasuryWithdrawals = "TreasuryWithdrawals",
  NoConfidence = "NoConfidence",
  NewCommittee = "NewCommittee",
  NewConstitution = "NewConstitution",
  InfoAction = "InfoAction",
}

export enum GovernanceActionField {
  Input = "input",
  TextArea = "textarea",
}

export type FieldSchema = {
  component: GovernanceActionField | [GovernanceActionField];
  labelI18nKey: NestedKeys<typeof en>;
  placeholderI18nKey: NestedKeys<typeof en>;
  tipI18nKey?: NestedKeys<typeof en>;
  rules: Omit<RegisterOptions, "valueAsNumber" | "valueAsDate" | "setValueAs">;
};

// Following properties are based on [CIP-108](https://github.com/Ryun1/CIPs/blob/governance-metadata-actions/CIP-0108/README.md)
// Which is an extension of [CIP-100](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0100)
// That classifies `body` parameter of the metadata
export type SharedGovernanceActionFieldSchema = {
  title: FieldSchema;
  abstract: FieldSchema;
  motivation: FieldSchema;
  rationale: FieldSchema;
};

export type InfoGovernanceActionFieldSchema = SharedGovernanceActionFieldSchema;
export type TreasuryGovernanceActionFieldSchema =
  SharedGovernanceActionFieldSchema &
    Partial<{
      receivingAddress: FieldSchema;
      amount: FieldSchema;
    }>;
export type NoConfidenceActionFieldSchema = SharedGovernanceActionFieldSchema;
export type NewCommitteeActionFieldSchema = SharedGovernanceActionFieldSchema &
  Partial<{
    prevGovernanceActionHash: FieldSchema;
    prevGovernanceActionIndex: FieldSchema;
    numerator: FieldSchema;
    denominator: FieldSchema;
    newCommitteeMembers: FieldSchema[];
    removeCommitteeMembers: FieldSchema[];
  }>;
export type HardForkInitiationActionFieldSchema =
  SharedGovernanceActionFieldSchema &
    Partial<{
      prevGovernanceActionHash: FieldSchema;
      prevGovernanceActionIndex: FieldSchema;
      major: FieldSchema;
      minor: FieldSchema;
    }>;
export type NewConstitutionActionFieldSchema =
  SharedGovernanceActionFieldSchema &
    Partial<{
      prevGovernanceActionHash: FieldSchema;
      prevGovernanceActionIndex: FieldSchema;
      constitutionUrl: FieldSchema;
      constitutionHash: FieldSchema;
      scriptHash: FieldSchema;
    }>;

export type GovernanceActionFieldSchemas =
  | InfoGovernanceActionFieldSchema & TreasuryGovernanceActionFieldSchema;

export type GovernanceActionFields = Record<
  GovernanceActionType.InfoAction | GovernanceActionType.TreasuryWithdrawals,
  GovernanceActionFieldSchemas
>;
