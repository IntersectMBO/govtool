import I18n from "@/i18n";

// `dataTestId`s become `<id>-checkbox` / `<id>-radio-wrapper` test ids, which
// the playwright governance action suite derives from the labels; keep them together.
export const GOV_ACTION_HISTORY_TYPE_FILTERS = [
  {
    value: "InfoAction",
    label: I18n.t("governanceActionHistoryList.filters.types.InfoAction"),
    dataTestId: "info",
  },
  {
    value: "TreasuryWithdrawals",
    label: I18n.t("governanceActionHistoryList.filters.types.TreasuryWithdrawals"),
    dataTestId: "treasury-withdrawals",
  },
  {
    value: "HardForkInitiation",
    label: I18n.t("governanceActionHistoryList.filters.types.HardForkInitiation"),
    dataTestId: "hard-fork-initiation",
  },
  {
    value: "NewCommittee",
    label: I18n.t("governanceActionHistoryList.filters.types.NewCommittee"),
    dataTestId: "update-committee",
  },
  {
    value: "NoConfidence",
    label: I18n.t("governanceActionHistoryList.filters.types.NoConfidence"),
    dataTestId: "motion-of-no-confidence",
  },
  {
    value: "NewConstitution",
    label: I18n.t("governanceActionHistoryList.filters.types.NewConstitution"),
    dataTestId: "new-constitution",
  },
  {
    value: "ParameterChange",
    label: I18n.t("governanceActionHistoryList.filters.types.ParameterChange"),
    dataTestId: "protocol-parameter-change",
  },
];

export const GOV_ACTION_HISTORY_STATUS_FILTERS = [
  { value: "live", label: I18n.t("governanceActionHistoryList.filters.statuses.live") },
  { value: "enacted", label: I18n.t("governanceActionHistoryList.filters.statuses.enacted") },
  {
    value: "ratified",
    label: I18n.t("governanceActionHistoryList.filters.statuses.ratified"),
  },
  { value: "expired", label: I18n.t("governanceActionHistoryList.filters.statuses.expired") },
];

export const GOV_ACTION_HISTORY_SORT_OPTIONS = [
  {
    value: "newestFirst",
    label: I18n.t("governanceActionHistoryList.sort.options.newestFirst.label"),
    displayLabel: I18n.t("governanceActionHistoryList.sort.options.newestFirst.displayLabel"),
    dataTestId: "newest-first",
  },
  {
    value: "oldestFirst",
    label: I18n.t("governanceActionHistoryList.sort.options.oldestFirst.label"),
    displayLabel: I18n.t("governanceActionHistoryList.sort.options.oldestFirst.displayLabel"),
    dataTestId: "oldest-first",
  },
  {
    value: "highestYesVotes",
    label: I18n.t("governanceActionHistoryList.sort.options.highestYesVotes.label"),
    displayLabel: I18n.t(
      "governanceActionHistoryList.sort.options.highestYesVotes.displayLabel",
    ),
    dataTestId: "highest-amount-of-yes-vote",
  },
];

// Same keys the previous package stored, so saved choices carry over.
export const GOV_ACTION_HISTORY_FILTERS_STORAGE_KEY = "governanceActionFilters";
export const GOV_ACTION_HISTORY_SORT_STORAGE_KEY = "governanceActionSort";

export const GOV_ACTION_HISTORY_ITEMS_PER_PAGE = 12;
