// SPEC §6. Ids and names are exact: Playwright test ids and pdf-ui magic
// values derive from them. Governance action type 5 is a UI stub, not seeded.

export const GOVERNANCE_ACTION_TYPES: ReadonlyArray<[number, string]> = [
  [1, 'Info Action'],
  [2, 'Treasury requests'],
  [3, 'Updates to the Constitution'],
  [4, 'Motion of No Confidence'],
  [6, 'Hard fork'],
];

export const BD_TYPES: ReadonlyArray<[number, string]> = [
  [1, 'Core'],
  [2, 'Research'],
  [3, 'Governance Support'],
  [4, 'Marketing & Innovation'],
  [5, 'None of these'],
];

export const BD_ROAD_MAPS: ReadonlyArray<[number, string]> = [
  [1, 'Scaling the L1 Engine'],
  [2, 'Architectural Excellence'],
  [3, 'Leios'],
  [4, 'Incoming Liquidity'],
  [5, 'L2 Expansion'],
  [6, 'Programmable Assets'],
  [7, 'Multiple Node Implementations'],
  [8, 'SPO Incentive Improvements'],
  [9, "It doesn't align"],
  [10, 'It supports the product roadmap'],
  [11, 'Developer / User Experience'],
];

export const BD_INTERSECT_COMMITTEES: ReadonlyArray<[number, string]> = [
  [1, 'Technical Steering Committee'],
  [2, 'Product Committee'],
  [3, 'Open Source Committee'],
  [4, 'Civics Committee'],
  [5, 'Membership & Community Committee'],
  [6, 'Budget Committee'],
  [7, 'Marketing Committee'],
  [8, 'Unsure'],
  [9, 'None'],
];

export const BD_CONTRACT_TYPES: ReadonlyArray<[number, string]> = [
  [1, 'Milestone Based Fixed Price'],
  [2, 'Time and Materials'],
  [3, 'Service Level Agreement'],
  [4, 'Other'],
  [5, 'Reimbursement'],
  [6, 'Intersect Procurement Process'],
];

/** (id, name, letter code, number code as a string: `036` keeps its zero). */
export const BD_CURRENCIES: ReadonlyArray<[number, string, string, string]> = [
  [1, 'United States Dollar', 'USD', '840'],
  [2, 'Euro', 'EUR', '978'],
  [3, 'Japanese Yen', 'JPY', '392'],
  [4, 'Australian Dollar', 'AUD', '036'],
  [5, 'Nepalese Rupee', 'NPR', '524'],
];

/** (id, name, alfa-2, alfa-3). */
export const COUNTRIES: ReadonlyArray<[number, string, string, string]> = [
  [1, 'Nepal', 'NP', 'NPL'],
  [2, 'Netherlands', 'NL', 'NLD'],
  [3, 'United States', 'US', 'USA'],
  [4, 'United Kingdom', 'GB', 'GBR'],
  [5, 'Canada', 'CA', 'CAN'],
  [6, 'Australia', 'AU', 'AUS'],
  [7, 'Germany', 'DE', 'DEU'],
  [8, 'France', 'FR', 'FRA'],
  [9, 'Japan', 'JP', 'JPN'],
  [10, 'South Korea', 'KR', 'KOR'],
];

/** Tables the seed owns; e2e truncation never touches them. */
export const LOOKUP_TABLES = [
  'governance_action_types',
  'bd_types',
  'bd_road_maps',
  'bd_intersect_committees',
  'bd_contract_types',
  'bd_currency_lists',
  'country_lists',
] as const;
