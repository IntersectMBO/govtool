import { col, defineResource, ResourceDef } from '../query/resource';

/** §5.2: seeded, publishedAt. */
export const GovernanceActionTypeResource = defineResource({
  name: 'governance-action-type',
  scalars: { gov_action_type_name: col.str('name', false) },
  publishedAt: true,
});

/** §5.5 lookup tables. */
export const BdTypeResource = defineResource({
  name: 'bd-type',
  scalars: { type_name: col.str('typeName', false) },
  publishedAt: true,
});

export const BdRoadMapResource = defineResource({
  name: 'bd-road-map',
  scalars: { roadmap_name: col.str('roadmapName', false) },
  publishedAt: true,
});

export const BdIntersectCommitteeResource = defineResource({
  name: 'bd-intersect-committee',
  scalars: { committee_name: col.str('committeeName', false) },
  publishedAt: true,
});

export const BdContractTypeResource = defineResource({
  name: 'bd-contract-type',
  scalars: { contract_type_name: col.str('contractTypeName', false) },
  publishedAt: true,
});

export const BdCurrencyResource = defineResource({
  name: 'bd-currency-list',
  scalars: {
    currency_name: col.str('currencyName', false),
    currency_letter_code: col.str('currencyLetterCode', false),
    currency_number_code: col.str('currencyNumberCode', false),
  },
  publishedAt: true,
});

export const CountryListResource = defineResource({
  name: 'country-list',
  scalars: {
    country_name: col.str('countryName', false),
    alfa_2_code: col.str('alfa2Code', false),
    alfa_3_code: col.str('alfa3Code', false),
  },
  publishedAt: true,
});

/** Route segment, descriptor and Prisma delegate name of each lookup list (§8.1, §8.9). */
export const LOOKUP_ROUTES: ReadonlyArray<{
  path: string;
  resource: ResourceDef;
  model:
    | 'governanceActionType'
    | 'bdType'
    | 'bdRoadMap'
    | 'bdIntersectCommittee'
    | 'bdContractType'
    | 'bdCurrency'
    | 'countryList';
}> = [
  { path: 'governance-action-types', resource: GovernanceActionTypeResource, model: 'governanceActionType' },
  { path: 'bd-types', resource: BdTypeResource, model: 'bdType' },
  { path: 'bd-road-maps', resource: BdRoadMapResource, model: 'bdRoadMap' },
  { path: 'bd-intersect-committees', resource: BdIntersectCommitteeResource, model: 'bdIntersectCommittee' },
  { path: 'bd-contract-types', resource: BdContractTypeResource, model: 'bdContractType' },
  { path: 'bd-currency-lists', resource: BdCurrencyResource, model: 'bdCurrency' },
  { path: 'country-lists', resource: CountryListResource, model: 'countryList' },
];
