// SPEC §6. Ids and names are exact: Playwright test ids and pdf-ui magic
// values derive from them. Governance action type 5 is a UI stub, not seeded.

export const GOVERNANCE_ACTION_TYPES: ReadonlyArray<[number, string]> = [
  [1, 'Info Action'],
  [2, 'Treasury requests'],
  [3, 'Updates to the Constitution'],
  [4, 'Motion of No Confidence'],
  [6, 'Hard fork'],
];

export const LOOKUP_TABLES = ['governance_action_types'] as const;
