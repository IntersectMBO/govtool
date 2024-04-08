import I18n from "@/i18n";

export const GOVERNANCE_ACTIONS_FILTERS = [
  {
    key: "NoConfidence",
    title: I18n.t("govActions.type.noConfidence.title"),
    label: I18n.t("govActions.type.noConfidence.label"),
  },
  {
    key: "NewCommittee",
    title: I18n.t("govActions.type.newCommittee.title"),
    label: I18n.t("govActions.type.newCommittee.label"),
  },
  {
    key: "NewConstitution",
    title: I18n.t("govActions.type.newConstitution.title"),
    label: I18n.t("govActions.type.newConstitution.label"),
  },
  {
    key: "HardForkInitiation",
    title: I18n.t("govActions.type.hardFork.title"),
    label: I18n.t("govActions.type.hardFork.label"),
  },
  {
    key: "ParameterChange",
    title: I18n.t("govActions.type.parameterChange.title"),
    label: I18n.t("govActions.type.parameterChange.label"),
  },
  {
    key: "TreasuryWithdrawals",
    title: I18n.t("govActions.type.treasuryWithdrawals.title"),
    label: I18n.t("govActions.type.treasuryWithdrawals.label"),
  },
  {
    key: "InfoAction",
    title: I18n.t("govActions.type.infoAction.title"),
    label: I18n.t("govActions.type.infoAction.label"),
  },
];
