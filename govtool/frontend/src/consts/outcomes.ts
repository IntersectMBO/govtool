import I18n from "@/i18n";

// `dataTestId`s become `<id>-checkbox` / `<id>-radio-wrapper` test ids, which
// the playwright outcomes suite derives from the labels; keep them together.
export const OUTCOMES_TYPE_FILTERS = [
  {
    value: "InfoAction",
    label: I18n.t("outcomesList.filters.types.InfoAction"),
    dataTestId: "info",
  },
  {
    value: "TreasuryWithdrawals",
    label: I18n.t("outcomesList.filters.types.TreasuryWithdrawals"),
    dataTestId: "treasury-withdrawals",
  },
  {
    value: "HardForkInitiation",
    label: I18n.t("outcomesList.filters.types.HardForkInitiation"),
    dataTestId: "hard-fork-initiation",
  },
  {
    value: "NewCommittee",
    label: I18n.t("outcomesList.filters.types.NewCommittee"),
    dataTestId: "update-committee",
  },
  {
    value: "NoConfidence",
    label: I18n.t("outcomesList.filters.types.NoConfidence"),
    dataTestId: "motion-of-no-confidence",
  },
  {
    value: "NewConstitution",
    label: I18n.t("outcomesList.filters.types.NewConstitution"),
    dataTestId: "new-constitution",
  },
  {
    value: "ParameterChange",
    label: I18n.t("outcomesList.filters.types.ParameterChange"),
    dataTestId: "protocol-parameter-change",
  },
];

export const OUTCOMES_STATUS_FILTERS = [
  { value: "live", label: I18n.t("outcomesList.filters.statuses.live") },
  { value: "enacted", label: I18n.t("outcomesList.filters.statuses.enacted") },
  {
    value: "ratified",
    label: I18n.t("outcomesList.filters.statuses.ratified"),
  },
  { value: "expired", label: I18n.t("outcomesList.filters.statuses.expired") },
];

export const OUTCOMES_SORT_OPTIONS = [
  {
    value: "newestFirst",
    label: I18n.t("outcomesList.sort.options.newestFirst.label"),
    displayLabel: I18n.t("outcomesList.sort.options.newestFirst.displayLabel"),
    dataTestId: "newest-first",
  },
  {
    value: "oldestFirst",
    label: I18n.t("outcomesList.sort.options.oldestFirst.label"),
    displayLabel: I18n.t("outcomesList.sort.options.oldestFirst.displayLabel"),
    dataTestId: "oldest-first",
  },
  {
    value: "highestYesVotes",
    label: I18n.t("outcomesList.sort.options.highestYesVotes.label"),
    displayLabel: I18n.t(
      "outcomesList.sort.options.highestYesVotes.displayLabel",
    ),
    dataTestId: "highest-amount-of-yes-vote",
  },
];

// Same keys the outcomes pillar package stored, so saved choices carry over.
export const OUTCOMES_FILTERS_STORAGE_KEY = "governanceActionFilters";
export const OUTCOMES_SORT_STORAGE_KEY = "governanceActionSort";

export const OUTCOMES_ITEMS_PER_PAGE = 12;
