# GovTool Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [Unreleased]

### Added

- Add metadata url and hash to drep details [Issue 2911](https://github.com/IntersectMBO/govtool/issues/2911)
- Add CC votes percentages, not voted and Ratification threshold
- Add support for submitting all 7 governance action types [Issue 2258](https://github.com/IntersectMBO/govtool/issues/2258)
- Add workflow to automatically update any of the @intersect.mbo package [Issue 2968](https://github.com/IntersectMBO/govtool/issues/2968)
- Add Propose Governance Action button in governance actions dashboard [Issue 1188](https://github.com/IntersectMBO/govtool/issues/1188)
- Add click handlers to non-interactive elements [Issue 2929](https://github.com/IntersectMBO/govtool/issues/2929)
- Allow searching for yourself in DRep Directory [Issue 2993](https://github.com/IntersectMBO/govtool/issues/2993)

### Fixed

- Fix calculating votes counting for governance actions
- Fix crashing backend on unhandled missing proposal from vote [Issue 2920](https://github.com/IntersectMBO/govtool/issues/2920)
- Remove abstain votes (not auto abstain) from total DRep stake
- Fix counting committee members [Issue 2948](https://github.com/IntersectMBO/govtool/issues/2948)

### Changed

- Change threshold visual representation in governance action votes
- Resize governance action details columns
- Update @intersect.mbo/pdf-ui to v0.6.0

### Removed

- Remove abstain from total DRep votes calculation

## [v2.0.11](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.11) 2025-02-04

### Added

-

### Fixed

- Fix displaying DRep with doNotList property as string
- Handle exception when no index is provided to /proposal/get endpoint [Issue 1841](https://github.com/IntersectMBO/govtool/issues/1841)
- Fix displaying vote pill on voted on cards
- Fix incorrect link to learn more about direct voters [Issue 2647](https://github.com/IntersectMBO/govtool/issues/2647)
- Fix missing No DRep found message on DRep Directory [Issue 2889](https://github.com/IntersectMBO/govtool/issues/2889)
- Fix displaying givenName placeholder instead of actual value on DRep card [Issue 2888](https://github.com/IntersectMBO/govtool/issues/2888)
- Fix executing insertBefore on undefined node [Issue 2878](https://github.com/IntersectMBO/govtool/issues/2878)

### Changed

- Change votes representation on Governance Actions [Issue 2880](https://github.com/IntersectMBO/govtool/issues/2880)
- Change vote rationale character limit to 10000 [Issue 2891](https://github.com/IntersectMBO/govtool/issues/2891)
- Move ratification threshold label below the voter type [Issue 2893](https://github.com/IntersectMBO/govtool/issues/2893)

### Removed

- Remove redundant sentry reports on handled wallet exceptions [Issue 2680](https://github.com/IntersectMBO/govtool/issues/2680)

## [v2.0.10](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.10) 2025-01-29

### Added

- Add exception handler on stake key voting power query execution [Issue 2757](https://github.com/IntersectMBO/govtool/issues/2757)
- Add script hash to new consitution governance action [Issue 2745](https://github.com/IntersectMBO/govtool/issues/2745)

### Fixed

-

### Changed

- Bump @intersect.mbo/pdf-ui to v0.5.11

### Removed

-

## [v2.0.9](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.9) 2025-01-24

### Added

-

### Fixed

- Fix opening IPFS links [Issue 2711](https://github.com/IntersectMBO/govtool/issues/2711)

### Changed

- Change labelling of governance action metadata anchor
- Change labelling and order of new constitution governance action details

### Removed

-

## [v2.0.8](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.8) 2025-01-23

### Added

- Add share DRep button to every DRep instead of only our own [Issue 2686](https://github.com/IntersectMBO/govtool/issues/2686)
- Show metadata anchor in Governance Action Details [Issue 2178](https://github.com/IntersectMBO/govtool/issues/2178)
- Handle unexpected drep info query result [Issue 2676](https://github.com/IntersectMBO/govtool/issues/2676)

### Fixed

- Fix usage of trim on missing label
- Fix blank screen when registering as a DRep [Issue 2408](https://github.com/IntersectMBO/govtool/issues/2408)
- Fix type mismatch between sql and haskell code for stake key address
- Handle missing api key exception [Issue 2683](https://github.com/IntersectMBO/govtool/issues/2683)

### Changed

- Bump cardano-node to 10.1.4
- Make CIP-129 governance identifiers the default ones

### Removed

- Remove logging to sentry for DRep registration transaction [Issue 2681](https://github.com/IntersectMBO/govtool/issues/2681)
- Remove logging to sentry when delegation transaction fails [Issue 2682](https://github.com/IntersectMBO/govtool/issues/2682)

## [v2.0.7](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.7) 2025-01-20

### Added

-

### Fixed

- Fix calculating DRep activity
- Fix fetching Governance Actions being navigated from dashboard

### Changed

- Bump @intersect.mbo/pdf-ui to v0.5.7

### Removed

-

## [v2.0.6](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.6) 2025-01-16

### Added

- Add support for base64 encoded images [Issue 2633](https://github.com/IntersectMBO/govtool/issues/2633)
- Add searching for metadata [Issue 2634](https://github.com/IntersectMBO/govtool/issues/2634)
- Allow delegation to inactive DRep [Issue 2589](https://github.com/IntersectMBO/govtool/issues/2589)

### Fixed

- Fix searching by full DRep IDs on wrong prefix cut [Issue 2639](https://github.com/IntersectMBO/govtool/issues/2639)
- Trim whitespace from search bar input [Issue 2472](https://github.com/IntersectMBO/govtool/issues/2472)

### Changed

-

### Removed

-

## [v2.0.5](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.5) 2025-01-10

### Added

-

### Fixed

- Fix counting submitted votes [Issue 2609](https://github.com/IntersectMBO/govtool/issues/2609)
- Fix opening relative paths in external links
- Fix passing random sorting to governance actions on disconnected wallet

### Changed

- Bump @intersect.mbo/pdf-ui to v0.5.6

### Removed

-

## [v2.0.4](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.4) 2025-01-07

### Added

-

### Fixed

- Fix CIP-129 DRep view identifier for script based DReps [Issue 2583](https://github.com/IntersectMBO/govtool/issues/2583)
- Fix fetching metadata from IPFS gateway
- Fix bad request on passing the random sorting to GA list request [Issue 2535](https://github.com/IntersectMBO/govtool/issues/2535)
- Fix wrong drep activity conditions

### Changed

- Reduce CSL version to 12.1.1 [Issue 2600](https://github.com/IntersectMBO/govtool/issues/2600)

### Removed

-

## [v2.0.3](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.3) 2024-12-30

### Added

-

### Fixed

- Fix calculating DRep live voting power [Issue 2460](https://github.com/IntersectMBO/govtool/issues/2460)
- Fix link and description validation [Issue 2403](https://github.com/IntersectMBO/govtool/issues/2403)
- Revert to drep_distr for providing active voting power
- Add rewards amount in the ada holder voting power
- Fix nested @value in jsonld metadatas [Issue 2509](https://github.com/IntersectMBO/govtool/issues/2509)
- Fix fetching latest voting anchor after DRep updates

### Changed

- Split New constitution on-chain details into tabs

### Removed

-

## [v2.0.2](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.2) 2024-12-23

### Added

- Add preprod adjustments [Issue 2547](https://github.com/IntersectMBO/govtool/issues/2547)

### Fixed

- Move matomo initalization out of the react code
- Fix some non-ipfs related errors while fetching the DRep images [Issue 2546](https://github.com/IntersectMBO/govtool/issues/2546)
- Remaining mobile responsiveness issue [Issue 2493](https://github.com/IntersectMBO/govtool/issues/2493)
- Fix searching by CIP-129 identifiers [Issue 2571](https://github.com/IntersectMBO/govtool/issues/2571)

### Changed

-

### Removed

-

## [v2.0.1](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.1) 2024-12-16

### Added

- Integrate matomo analytics

### Fixed

- Fix mzero parsing error on fetching the /proposal/list [Issue 2446](https://github.com/IntersectMBO/govtool/issues/2446)
- Fix scaling gov action votes on lower resolutions
- Fix storing url missing length validation [Issue 2044](https://github.com/IntersectMBO/govtool/issues/2044)
- Fix governance action details page crash on missing data [Issue 2511](https://github.com/IntersectMBO/govtool/issues/2511)
- Fix wrong voting conditions for drep

### Changed

-

### Removed

-

## [v2.0.0](https://github.com/IntersectMBO/govtool/releases/tag/v2.0.0) 2024-12-10

### Added

-

### Fixed

-

### Changed

- Bump @intersect.mbo/pdf-ui to v0.5.5
- Change limit of active db connections

### Removed

-

## [v1.0.30](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.30) 2024-12-10

### Added

- Add stake key registration certificate to voting and gov action proposal if not registered [Issue 2490](https://github.com/IntersectMBO/govtool/issues/2490)

### Fixed

- Fix vulnerabilities in dependencies [Issue 2192](https://github.com/IntersectMBO/govtool/issues/2192)
- Optimize resources usage in frontend service build [Issue 2192](https://github.com/IntersectMBO/govtool/issues/2192)
- Fix sentry reports on multiple reloads of Governance Actions [Issue 2446](https://github.com/IntersectMBO/govtool/issues/2446)

### Changed

-

### Removed

-

## [v1.0.29](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.29) 2024-12-05

### Added

- Add support for displaying CIP-119 DRep images [Issue 1806](https://github.com/IntersectMBO/govtool/issues/1806)

### Fixed

- Fix fetching voting power of newly registerd DRep [Issue 2407](https://github.com/IntersectMBO/govtool/issues/2407)
- Fix inconsistent voting status [Issue 1713](https://github.com/IntersectMBO/govtool/issues/1713)
- Fix removing a child (link) when is not registed in DOM [Issue 2398](https://github.com/IntersectMBO/govtool/issues/2398)
- Fix blank screen on DRep delegation when UTXos are missing [Issue 2408](https://github.com/IntersectMBO/govtool/issues/2408)
- Fix broken guides links [Issue 2417](https://github.com/IntersectMBO/govtool/issues/2417)
- Fix double signing of transaction [Issue 2478](https://github.com/IntersectMBO/govtool/issues/2478)

### Changed

- Bump cardano-db-sync to 13.6.0.4 [Issue 2411](https://github.com/IntersectMBO/govtool/issues/2411)
- Bump @intersect.mbo/pdf-ui to v0.5.3
- Bump @intersect.mbo/pdf-ui to v0.5.4

### Removed

-

## [v1.0.28](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.28) 2024-11-26

### Added

- Add more useful metrics to the backend GET /network/metrics endpoint [Issue 2010](https://github.com/IntersectMBO/govtool/issues/2010)

### Fixed

- Fix ada holder voting power calculation
- Fix wrong statuses on past DRep info
- Fix listing voted-on governance actions [Issue 2379](https://github.com/IntersectMBO/govtool/issues/2379)
- Fix wronly displayed markdown on slider card [Issue 2263](https://github.com/IntersectMBO/govtool/issues/2316)
- fix ada quantities format to avoid thousands when the total is 0 [Issue 2372](https://github.com/IntersectMBO/govtool/issues/2382)
- fix inconsistent display of delegated DRep card during delegation [Issue 2332](https://github.com/IntersectMBO/govtool/issues/2332)

### Changed

- Bump CSL version to 13.1.0 [Issue 2169](https://github.com/IntersectMBO/govtool/issues/2169)
- Display supporting links below labels than in same row [Issue 2391](https://github.com/IntersectMBO/govtool/issues/2391)
- change link to support page [Issue 2292](https://github.com/IntersectMBO/govtool/issues/2292)
- Bump @intersect.mbo/pdf-ui to v0.5.0

### Removed

-

## [v1.0.27](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.27) 2024-11-19

### Added

- Handle displaying votes based on bootstrap phase, full governance and security groups [Issue 2316](https://github.com/IntersectMBO/govtool/issues/2316)
- Add support for displaying Motion of No Confidence Governance Action [Issue 1597](https://github.com/IntersectMBO/govtool/issues/1597)
- Add support for displaying Update committee/threshold Governance Action [Issue 1598](https://github.com/IntersectMBO/govtool/issues/1598)
- Add support for displaying New Constitution and/or Guardrails Script Governance Action [Issue 1599](https://github.com/IntersectMBO/govtool/issues/1598)
- Add support for ipfs in metadata validation service [Issue 1616](https://github.com/IntersectMBO/govtool/issues/1616)
- Add support for displaying array of treasury withdrawals [Issue 1602](https://github.com/IntersectMBO/govtool/issues/1602)

### Fixed

- Fix submitting treasury governance action [Issue 1845](https://github.com/IntersectMBO/govtool/issues/1845)
- Fix failing github action workflow [Issue 2277](https://github.com/IntersectMBO/govtool/issues/2277)

### Changed

- Bumped Cardano node version to `10.1.2`.
- Bumped Cardano DB Sync version to `13.6.0.1`.
- Make calculation of a DRep voting power on the backend [Issue 1960](https://github.com/IntersectMBO/govtool/issues/1960)

### Removed

-

## [v1.0.26](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.26) 2024-11-07

### Added

- add support for CIP-129 governance identifiers [Issue 2183](https://github.com/IntersectMBO/govtool/issues/2183)
- Add label to supporting links in Governance Action details [Issue 2305](https://github.com/IntersectMBO/govtool/issues/2305)

### Fixed

- Fix certificates order where vote delegation cert is before the DRep registration cert [Issue 2333](https://github.com/IntersectMBO/govtool/issues/2333)

### Changed

- Change multilanguage support to use json file [Issue 2325](https://github.com/IntersectMBO/govtool/issues/2325)
- Display full Governance Action IDs on desktop
- Support space on given name [Issue 2276](https://github.com/IntersectMBO/govtool/issues/2276)
- Display ADA in 'en-US' format [Issue 2305](https://github.com/IntersectMBO/govtool/issues/2305)

### Removed

-

## [v1.0.25](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.25) 2024-11-04

### Added

-

### Fixed

- Fix searching by DRep Given name
- Fix displaying the wallet connected modal
- Fix navigating to DRep details [Issue 2307](https://github.com/IntersectMBO/govtool/issues/2307)

### Changed

-

### Removed

-

## [v1.0.24](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.24) 2024-10-31

### Added

-

### Fixed

- Fix infinite DRep list loading [Issue 2285](https://github.com/IntersectMBO/govtool/issues/2285)

### Changed

-

### Removed

-

## [v1.0.23](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.23) 2024-10-29

### Added

- Add searching for DRep and Proposal metadatas [Issue 1893](https://github.com/IntersectMBO/govtool/issues/1783)

### Fixed

- Fix validating metadata against the CIP standard [Issue 2233](https://github.com/IntersectMBO/govtool/issues/2233)
- Fix counting the CC votes [Issue 2247](https://github.com/IntersectMBO/govtool/issues/2247)

### Changed

-

### Removed

-

## [v1.0.22](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.22) 2024-10-24

### Added

-

### Fixed

- Fix unwanted horizontal page scroll on Governance Actions page [Issue 1897](https://github.com/IntersectMBO/govtool/issues/1897)
- Fix duplicate testIds for reference errors and hints in DRep metadata form [Issue 1965](https://github.com/IntersectMBO/govtool/issues/1965)
- Eliminate duplicate DReps in the DRep Directory [Issue 2171](https://github.com/IntersectMBO/govtool/issues/2171)
- Handle script based DReps [Issue 1951](https://github.com/IntersectMBO/govtool/issues/1951)
- Fix displaying protocol parameter cost models [Issue 2191](https://github.com/IntersectMBO/govtool/issues/2191)

### Changed

- Bump to use Cardano Node `10.0.0-pre`
- Changed copy for no DRep found via search

### Removed

-

## [v1.0.21](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.21) 2024-10-15

### Added

-

### Fixed

- Fix counting epoch boundaries for Governance Actions [Issue 2125](https://github.com/IntersectMBO/govtool/issues/2125)
- Fix displaying the SPO Votes [Issue 2085](https://github.com/IntersectMBO/govtool/issues/2085)
- Fix counting ada holder voting power [Issue 2000](https://github.com/IntersectMBO/govtool/issues/2000)

### Changed

-

### Removed

-

## [v1.0.20](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.20) 2024-10-03

### Added

- Add useful external links to home page and dashboard [Issue 1995](https://github.com/IntersectMBO/govtool/issues/1995)

### Fixed

- Add missing testIds for submitted votes [Issue 1875](https://github.com/IntersectMBO/govtool/issues/1875)
- Provide workaround for iOS for downloading metadata on iOS [Issue 1989](https://github.com/IntersectMBO/govtool/issues/1989)
- Fix infinite loading in DRep Directory [Issue 2090](https://github.com/IntersectMBO/govtool/issues/2090)

### Changed

- Change constitutional committee vote totals to be constitutional for yes and unconstitutional for no [Issue 2062](https://github.com/IntersectMBO/govtool/issues/2062)
- Bump @intersect.mbo/pdf-ui to v0.4.0
- Include @language property in generated jsonld files [Issue 1856](https://github.com/IntersectMBO/govtool/issues/1856)

### Removed

-

## [v1.0.19](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.19) 2024-09-19

### Added

-

### Fixed

- Fix private policy link and get support link
- Fixed poor UTxO management when building transactions [Issue 2059](https://github.com/IntersectMBO/govtool/issues/2059)

### Changed

- Bump cardano-db-sync 13.5.0.2 [Issue 1945](https://github.com/IntersectMBO/govtool/issues/1945)
- Add Mainnet link to network banner [Issue 2002](https://github.com/IntersectMBO/govtool/issues/2002)
- Bump CSL version to 12.1.0
- Add random sorting as default sorting for DRep list [Issue 2013](https://github.com/IntersectMBO/govtool/issues/2013)

### Removed

-

## [v1.0.18](https://github.com/IntersectMBO/govtool/releases/tag/v1.0.18) 2024-09-12

### Added

- Add script for Matomo Analytics [Issue 1817](https://github.com/IntersectMBO/govtool/issues/1817)

### Fixed

- Correctly show all kinds of votes in the modal showing vote numbers [Issue 1941](https://github.com/IntersectMBO/govtool/issues/1941)
- Fixed terms and conditions link [Issue 1968](https://github.com/IntersectMBO/govtool/issues/1968)
- Hide Delegate button in DRep list and details if user has already delegated to this DRep [Issue 1982](https://github.com/IntersectMBO/govtool/issues/1982)
- Fix condition for disabling voting on different GA types [Issue 2008](https://github.com/IntersectMBO/govtool/issues/2008)
- Fix incorrect copy (ex. github) to (e.g. github) [Issue 1748](https://github.com/IntersectMBO/govtool/issues/1748)
- Fix broken translation in DRep Details [Issue 2036](https://github.com/IntersectMBO/govtool/issues/2036)

### Changed

- Bump @intersect.mbo/pdf-ui to v0.3.9
- Changed misleading text for direct voter registration [Issue 1976](https://github.com/IntersectMBO/govtool/issues/1976)
- Change base repo README header image to have correct branding and reflect mainnet launch.

## [sancho-v1.0.17](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.17) 2024-09-05

### Added

-

### Fixed

- Make testIds for link and identity references in drep form unique [Issue 1928](https://github.com/IntersectMBO/govtool/issues/1928)
- Fix saving the Do Not List checkbox value on DRep registration [Issue 1940](https://github.com/IntersectMBO/govtool/issues/1940)

### Changed

- Make displaying own DRep in DRep directory available [Issue 1934](https://github.com/IntersectMBO/govtool/issues/1934)

### Removed

-

## [sancho-v1.0.16](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.16) 2024-09-04

### Fixed

- Fix incorrect documentation link for "Learn More" button on proposal [Issue 1877](https://github.com/IntersectMBO/govtool/issues/1877)
- Fix getVotes sql to query time from vote tx instead of govAaction tx [Issue 1925](https://github.com/IntersectMBO/govtool/issues/1925)
- Map references from validation service to format expected by FE [Issue 1924](https://github.com/IntersectMBO/govtool/issues/1924)

### Changed

- Change the spelling of ADA to Ada or ada [Issue 1916](https://github.com/IntersectMBO/govtool/issues/1916)

### Removed

-

## [sancho-v1.0.15](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.15) 2024-09-03

### Added

- Add missing test DRep Details test IDs [Issue 1863](https://github.com/IntersectMBO/govtool/issues/1863)
- Add missing system banners test IDs [Issue 1839](https://github.com/IntersectMBO/govtool/issues/1839)

### Fixed

- Delete duplicate text on DRep registration form [Issue 1847](https://github.com/IntersectMBO/govtool/issues/1847)
- Fix modal content invisible on ios [Issue 1842](https://github.com/IntersectMBO/govtool/issues/1842)
- Fix counting votes by CC committee members and SPOs [Issue 1838](https://github.com/IntersectMBO/govtool/issues/1838)
- Fix displaying non relevant data in protocol parameter change Governance Action [Issue 1601](https://github.com/IntersectMBO/govtool/issues/1601)
- Fix voting on info actions in bootstrapping phase [Issue 1876](https://github.com/IntersectMBO/govtool/issues/1876)
- Fix missing DRep name whitespace validation [Issue 1873](https://github.com/IntersectMBO/govtool/issues/1873)
- Fix displaying wrongly formatted Governance Action ID [Issue 1866](https://github.com/IntersectMBO/govtool/issues/1866k)
- Make payment address optional in DRep registration and edit form [Issue 1871](https://github.com/IntersectMBO/govtool/issues/1871)
- Fix displaying wrongly formatted Governance Action ID [Issue 1866](https://github.com/IntersectMBO/govtool/issues/1866)
- Fix displaying the proper Governance Action Details on renavigating between pages
- Fix displaying protocol params Governance Action details when metadata validation fails [Issue 1889](https://github.com/IntersectMBO/govtool/issues/1889)
- Fix runtime error when navigating to GA details from Voted on by me tab [Issue 1910](https://github.com/IntersectMBO/govtool/issues/1910)

### Changed

- Replace diff library to avoid usage of `--force` in package installation
- Bump @intersect.mbo/pdf-ui to v0.3.8
- Change logo to Cardano GovTool [Issue 1851](https://github.com/IntersectMBO/govtool/issues/1851)
- Change copy to Cardano GovTool [Issue 1852](https://github.com/IntersectMBO/govtool/issues/1852)
- Replace mocked MrDRep label in DRep retired card with given name if exists, or make the description more neutral if the given name is not provided [Issue 1887](https://github.com/IntersectMBO/govtool/issues/1887)
- Change home page hero copy [Issue 1903](https://github.com/IntersectMBO/govtool/issues/1903)
- Bump cardano-node to 9.1.1 [Issue 1895](https://github.com/IntersectMBO/govtool/issues/1895)
- Bump cardano-db-sync 13.5.0.1 [Issue 1906](https://github.com/IntersectMBO/govtool/issues/1906)
- Adjust CIP119 support to match final designs [Issue 1850](https://github.com/IntersectMBO/govtool/issues/1850)

## [sancho-v1.0.14](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.14) 2024-08-26

### Added

- Add support for displaying protocol parameters governance actions [Issue 1601](https://github.com/IntersectMBO/govtool/issues/1601)
- Add SPO and CC committee total votes to gov actions [Issue 1704](https://github.com/IntersectMBO/govtool/issues/1704)
- Add support for hard fork initiation governance action details [Issue 1600](https://github.com/IntersectMBO/govtool/issues/1600)
- Add support for hard fork initiation previous governance action data [Issue 1600](https://github.com/IntersectMBO/govtool/issues/1600)
- Add support for CIP-119 on the backend and metadata validation [Issue 1758](https://github.com/IntersectMBO/govtool/issues/1758)
- Add support for CIP-119 on the frontend [Issue 1760](https://github.com/IntersectMBO/govtool/issues/1758)
- Add support for HF Initiation and Protocol Parameter Change governance action builders [Issue 1600](https://github.com/IntersectMBO/govtool/issues/1600) & [Issue 1601](https://github.com/IntersectMBO/govtool/issues/1601)

### Fixed

- Fix typescript bug leading to runtime error when entering Governance Action details page via direct link [Issue 1801](https://github.com/IntersectMBO/govtool/issues/1801)
- Fix accessing missing epochParams drep_deposit [Issue 1733](https://github.com/IntersectMBO/govtool/issues/1733)

### Changed

- Bump @intersect.mbo/pdf-ui to v0.3.7
- Decrease level of wallet related sentry reports [Issue 1699](https://github.com/IntersectMBO/govtool/issues/1699)
- Bump cardano-db-sync to 13.3.0.0 [Issue 1809](https://github.com/IntersectMBO/govtool/issues/1809)
- Update Gitbook links [Issue 1774](https://github.com/IntersectMBO/govtool/issues/1774)

## [sancho-v1.0.13](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.13) 2024-08-22

### Added

- Unsancho GovTool [Issue 1637](https://github.com/IntersectMBO/govtool/issues/1637)
- Add network metrics model to frontend service
- Added workflows to add labels to issues
- Make voting on governance actions enabled based on protocol version [Issue 1703](https://github.com/IntersectMBO/govtool/issues/1703)
- Add banner informing about network [Issue 1702](https://github.com/IntersectMBO/govtool/issues/1702)
- Add banner informing about bootstrapping phase [Issue 1708](https://github.com/IntersectMBO/govtool/issues/1708)

### Fixed

- Incorrect copy on DRep votes + other minor copy spelling mistakes
- Remove incorrect @value property to fix validating the metadata body [Issue 1687](https://github.com/IntersectMBO/govtool/issues/1687)
- Fix PDF not enabled in default deployments
- Fix displaying incorrect connected to Voltaire GovTool network
- Runtime error at /edit_drep when metadata incorrect [Issue 1675](https://github.com/IntersectMBO/govtool/issues/1675)
- Restrict access to registration routes when user s already registered and to retirement routes when the use is not registered [Issue 1183](https://github.com/IntersectMBO/govtool/issues/1183)
- Add missing data-testid attributes [Issue 1050](https://github.com/IntersectMBO/govtool/issues/1050)

### Changed

- Changed stake key registration to use the newer Conway type of certificate
- Changed and improved issue templates
- Changed wording and image in base README
- Change link to docs

## [sancho-v1.0.12](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.12) 2024-08-01

### Added

- Add network name to GET /network/metrics [Issue 1644](https://github.com/IntersectMBO/govtool/issues/1644)

### Fixed

- Fix displaying Governance Action & DRep metadatas

### Changed

- Change link to propose a governace action docs [Issue 1132](https://github.com/IntersectMBO/govtool/issues/1132)
- Change link to docs regarding DReps [Issue 1130](https://github.com/IntersectMBO/govtool/issues/1130)
- Change link to view governance actions docs [Issue 1131](https://github.com/IntersectMBO/govtool/issues/1131)
- Change link to delegate voting power docs, [Issue 1654](https://github.com/IntersectMBO/govtool/issues/1654)
- Make all the frontend build arguments mandatory [Issue 1642](https://github.com/IntersectMBO/govtool/issues/1642), [Issue 1643](https://github.com/IntersectMBO/govtool/issues/1643)
- Breaking! Remove usage of metadata validation service in Haskell Backend
- Bump @intersect.mbo/pdf-ui to v0.3.3

## [sancho-v1.0.11](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.11) 2024-07-30

### Added

-

### Fixed

- Fix unbound variable in the resync cardano node github workflow [Issue 1624](https://github.com/IntersectMBO/govtool/issues/1624)

### Changed

- bump @intersect.mbo/pdf-ui to v0.3.2

## [sancho-v1.0.10](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.10) 2024-07-29

### Added

- Add redirect to the connected DRep Directory page if a connected user enters a non-connected DRep Directory url [Issue 1117](https://github.com/IntersectMBO/govtool/issues/1117)
- Add PDF_API_URL to the frontend config [Issue 1575](https://github.com/IntersectMBO/govtool/issues/1575)
- Provide DB-Sync Postgres envs to the backend config
- Add redirects to cards on Home after user connects [Issue 1442](https://github.com/IntersectMBO/govtool/issues/1442)
- Add support for CIP-100 for context to Governance Action Vote [Issue 1582](https://github.com/IntersectMBO/govtool/issues/1582)

### Fixed

- Fix querying metadata-validation service from haskell backend [Issue 1612](https://github.com/IntersectMBO/govtool/issues/1612)
- Fix thread killed by timeout manager [Issue 1417](https://github.com/IntersectMBO/govtool/issues/1417)
- Changed use new CSL UTxO selection to take into account change

### Changed

- Set Proposal Discussion Forum Pillar enabled by default [Issue 1501](https://github.com/IntersectMBO/govtool/issues/1501)
- bump @intersect.mbo/pdf-ui to v0.3.1

## [sancho-v1.0.9](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.9) 2024-07-16

### Added

- Added 'sentryenv' field in backend config file [Issue 1401](https://github.com/IntersectMBO/govtool/issues/1401)
- Add wallet connector package [Issue 898](https://github.com/IntersectMBO/govtool/issues/898)
- Initialize Usersnap into App [Issue 546](https://github.com/IntersectMBO/govtool/issues/546)
- Integrate frontend with metadata validation service [Issue 617](https://github.com/IntersectMBO/govtool/issues/617)
- Implement a loading modal for the validation of the metadata [Issue 646](https://github.com/IntersectMBO/govtool/issues/646)
- Integrate ga displaying metadata validation with the validation service [Issue 712](https://github.com/IntersectMBO/govtool/issues/712)
- added `bio` `dRepName` `email` `references` `metadataValid` and `metadataStatus` fields to `drep/list`
- added `metadatavalidationmaxconcurrentrequests` field to the backend config
- added `metadata/validate` endpoint [Issue 876](https://github.com/IntersectMBO/govtool/issues/876)
- added pagination to `drep/list` [Issue 756](https://github.com/IntersectMBO/govtool/issues/756)
- added search query param to the `drep/getVotes` [Issue 640](https://github.com/IntersectMBO/govtool/issues/640)
- added filtering and sorting capabilities to the `drep/list` [Issue 722](https://github.com/IntersectMBO/govtool/issues/722)
- added drepView and txHash to the `ada-holder/get-current-delegation` [Issue 689](https://github.com/IntersectMBO/govtool/issues/689)
- addded latestTxHash to the `drep/info` and `drep/list` endpoints [Issue 627](https://github.com/IntersectMBO/govtool/issues/627)
- added `txHash` to `drep/getVotes` [Issue 626](https://github.com/IntersectMBO/govtool/issues/626)
- added `references` to all proposal related endpoints
- added `epochNo` and `date` to `drep/getVotes` and `proposal/get`
- Added `isRegisteredAsSoleVoter` and `wasRegisteredAsSoleVoter` fields to the drep/info response [Issue 212](https://github.com/IntersectMBO/govtool/issues/212)
- Abandoning registration as DRep [Issue 151](https://github.com/IntersectMBO/govtool/issues/151)
- Abandoning GA creation [Issue 359](https://github.com/IntersectMBO/govtool/issues/359)
- Choose GA type - GA Submiter [Issue 358](https://github.com/IntersectMBO/govtool/issues/358)
- Create Automated Voting Options component [Issue 216](https://github.com/IntersectMBO/govtool/issues/216)
- Change step 3 components [Issue 152](https://github.com/intersectMBO/govtool/issues/152)
- Add possibility to vote on behalf of myself - Sole Voter [Issue 119](https://github.com/IntersectMBO/govtool/issues/119)
- Create DRep registration page about roles [Issue 205](https://github.com/IntersectMBO/govtool/issues/205)
- Create Checkbox component. Improve Field and ControlledField [Issue 177](https://github.com/IntersectMBO/govtool/pull/177)
- Vitest unit tests added for utils functions [Issue 81](https://github.com/IntersectMBO/govtool/issues/81)
- i18next library added to FE [Issue 80](https://github.com/IntersectMBO/govtool/issues/80)
- Add possibility to vote on behalf of myself - Sole Voter [Issue 119](https://github.com/IntersectMBO/govtool/issues/119)
- Added Nix Flakes configurtion to handle unified developers setup [Issue 526](https://github.com/IntersectMBO/govtool/issues/526)
- Add missing test to utils [Issue 500](https://github.com/IntersectMBO/govtool/issues/500).
- DRep metadata builder [Issue 497](https://github.com/IntersectMBO/govtool/issues/497)
- Add generate jsonld function [Issue 451](https://github.com/IntersectMBO/govtool/issues/451)
- Create GA review subbmision page [Issue 362](https://github.com/IntersectMBO/govtool/issues/362)
- Create GA creation form [Issue 360](https://github.com/IntersectMBO/govtool/issues/360)
- Create TextArea [Issue 110](https://github.com/IntersectMBO/govtool/issues/110)
- Choose GA type - GA Submiter [Issue 358](https://github.com/IntersectMBO/govtool/issues/358)
- Add on-chain inputs validation [Issue 377](https://github.com/IntersectMBO/govtool/issues/377)
- Add hash and validation of the metadata [Issue 378](https://github.com/IntersectMBO/govtool/issues/378)
- Add githubusercontent.com and ipfs.io to content security policy header [Issue 451](https://github.com/IntersectMBO/govtool/issues/451)
- Add frontend test workflow on github actions [Issue 500](https://github.com/IntersectMBO/govtool/issues/500)
- Add type check & lint to github actions [Issue 512](https://github.com/IntersectMBO/govtool/issues/512)
- Add eslint & prettier to frontend package [Issue 166](https://github.com/IntersectMBO/govtool/issues/166)
- Add DRep list pagination [Issue 740](https://github.com/IntersectMBO/govtool/issues/740)
- Add PDF pillar [Issue 1090](https://github.com/IntersectMBO/govtool/issues/1090)
- Replace govtool-wrapper governance action creation in favor of pdf-pillar [Issue 1284](https://github.com/IntersectMBO/govtool/issues/1284)
- Add sentry environment config [Issue 1324](https://github.com/IntersectMBO/govtool/issues/1324)
- Add proposal discussion pillar to home page [Issue 1431](https://github.com/IntersectMBO/govtool/issues/1431)
- Enable coverage tests report

### Fixed

- silenced `Thread killed by timeout manager` sentry log [Issue 1417](https://github.com/IntersectMBO/govtool/issues/1417)
- silenced `Warp: Client closed connection prematurely` error [Issue 1422](https://github.com/IntersectMBO/govtool/issues/1422)
- backend is now compiled with -threaded [Issue 1148](https://github.com/IntersectMBO/govtool/issues/1148)
- drep/get-voting-power no longer throws 500 for non-existing dreps. Instead it returns 0 [Issue 1093](https://github.com/IntersectMBO/govtool/issues/1093)
- proposal/list no longer throws 500 error when proposal's url is incorrect [Issue 1073](https://github.com/IntersectMBO/govtool/issues/1073)
- drep/list sql fix (now the drep type is correct) [Issue 957](https://github.com/IntersectMBO/govtool/issues/957)
- drep/list sql fix (now the latest tx date is correct) [Issue 826](https://github.com/IntersectMBO/govtool/issues/826)
- drep/info no longer returns null values [Issue 720](https://github.com/IntersectMBO/govtool/issues/720)
- drep/getVotes no longer returns 500 [Issue 685](https://github.com/IntersectMBO/govtool/issues/685)
- drep/info no longer returns 500 [Issue 676](https://github.com/IntersectMBO/govtool/issues/676)
- proposal/list search is case insensitive now [Issue 582](https://github.com/IntersectMBO/govtool/issues/582)
- proposal/list now takes optional `search` query param [Issue 566](https://github.com/IntersectMBO/govtool/issues/566)
- Fix possible sql error when there would be no predefined drep voting pwoer [Issue 501](https://github.com/IntersectMBO/govtool/issues/501)
- Fix drep type detection when changing metadata [Issue 333](https://github.com/IntersectMBO/govtool/issues/333)
- Fix make button disble when wallet tries connect [Issue 265](https://github.com/IntersectMBO/govtool/issues/265)
- Fix drep voting power calculation [Issue 231](https://github.com/IntersectMBO/govtool/issues/231)
- Fix proposal/list and network/metrics bug that appeared when noone has delegated their funds either to drep_always_abstain or drep_always_no_confidence [Issue 231](https://github.com/IntersectMBO/govtool/issues/231)
- Fix GA details [Issue 272](https://github.com/IntersectMBO/govtool/issues/272)
- Fix copy for maintenance page [Issue 180](https://github.com/IntersectMBO/govtool/issues/180)
- Fix misleading metadata hash text [Issue 90](https://github.com/IntersectMBO/govtool/issues/90)
- Fixed vote calculation problems related to NoConfidence DRep [Issue 59](https://github.com/IntersectMBO/govtool/issues/59)
- Fixed ada-holder/get-current-delegation error when delegated to NoConfidence or AlwaysAbstain dreps. [Issue 82](https://github.com/IntersectMBO/govtool/issues/82)
- Fixed deployment scripts to address [Issue 171](https://github.com/IntersectMBO/govtool/issues/171).
- Fixed get drep voting power incorrectly executed endpoint [Issue 280](https://github.com/IntersectMBO/govtool/issues/280).
- Fixed CSP settings to allow error reports with Sentry [Issue 291](https://github.com/IntersectMBO/govtool/issues/291).
- Fix frontend package tests [Issue 500](https://github.com/IntersectMBO/govtool/issues/500).
- Fix all the existing eslint errors [Issue 514](https://github.com/IntersectMBO/govtool/issues/514)
- Fix all the existing typescript errors [Issue 514](https://github.com/IntersectMBO/govtool/issues/514)
- Fix endless spinner on a dashboard [Issue 539](https://github.com/IntersectMBO/govtool/issues/539)
- Remove wrongly appended `Yourself` filter on DRep Directory [Issue 1028](https://github.com/IntersectMBO/govtool/issues/1028)
- Fix validation of uris in metadata [Issue 1011](https://github.com/IntersectMBO/govtool/issues/1011)
- Fix wrong link to the GA Details once it is in progress [Issue 1252](https://github.com/IntersectMBO/govtool/issues/1252)
- Fix validation of the GAs with missing references [Issue 1282](https://github.com/IntersectMBO/govtool/issues/1282)
- Fix displaying the GA Markdowns [Issue 1244](https://github.com/IntersectMBO/govtool/issues/1244)
- Fix app crash on voting on the GA without the connected wallet before [Issue 1313](https://github.com/IntersectMBO/govtool/issues/1313)
- Fix the navigation to Home from Proposal pillar on disconnected wallet [Issue 1355](https://github.com/IntersectMBO/govtool/issues/1355)
- Fix navigation over Proposal discussion forum pillar [Issue 1436](https://github.com/IntersectMBO/govtool/issues/1436)
- Fix building the frontend image [Issue 1583](https://github.com/IntersectMBO/govtool/issues/1583)
- Correct text of the governance action type [Issue 651](https://github.com/IntersectMBO/govtool/issues/651)
- Fix displaying modals to not block signing transactions [Issue 710](https://github.com/IntersectMBO/govtool/issues/710)

### Changed

- `proposal.about` changed to `proposal.abstract`
- `drep/info` now returns 4 different tx hashes instead of one latest tx hash [Issue 688](https://github.com/IntersectMBO/govtool/issues/688)
- `proposal/list` allows user to search by tx hash [Issue 603](https://github.com/IntersectMBO/govtool/issues/603)
- `proposal/list` returns additional data such ass `expiryEpochNo`, `createdEpochNo`, `title`, `about`, `motivation`,
  `rationale`. `TreasuryWithdrawals` GAs also got nicely formated details. [Issue 372](https://github.com/IntersectMBO/govtool/issues/372)
- `drep/list` now return also `status` and `type` fields. Also it now returns the retired dreps, and you can search for given drep by name using optional query parameter. If the drep name is passed exactly, then you can even find a drep that's sole voter. [Issue 446](https://github.com/IntersectMBO/govtool/issues/446)
- `drep/list` and `drep/info` endpoints now return additional data such as metadata url and hash, and voting power [Issue 223](https://github.com/IntersectMBO/govtool/issues/223)
- `drep/info` now does not return sole voters (dreps without metadata) [Issue 317](https://github.com/IntersectMBO/govtool/issues/317)
- `isRegistered` and `wasRegistered` fields in the drep/info endpoint changed to `isRegisteredAsDRep` and `wasRegisteredAsDRep` respectively [Issue 212](https://github.com/IntersectMBO/govtool/issues/212)
- Update Cardano-Serialization-Lib to 12.0.0-alpha.16 [Issue 156](https://github.com/IntersectMBO/govtool/issues/156)
- Changed and improved working conventions docs, PR template and codeowners file, addressing [Issue 88](https://github.com/IntersectMBO/govtool/issues/88).
- Changed Node version from 8.7.1-pre to 8.8.0-pre and DbSync version from sancho-2-3-0 to sancho-4-0-0-fix-config, addressing also [Issue 181](https://github.com/IntersectMBO/govtool/issues/181).
- Reorganized repository to fit new conventions [Issue 85](https://github.com/IntersectMBO/govtool/issues/85).
- Renamed project from VVA to GovTool [Issue 97](https://github.com/IntersectMBO/govtool/issues/97).
- (`docs/update-working-conventions`) Addressing [Issue 25](https://github.com/IntersectMBO/govtool/issues/25) changed working conventions documentation to improve intended flows.
- Adjusted Nix configuration to meet projects needs [Issue 187](https://github.com/IntersectMBO/govtool/issues/187).
- Integrated OAuth to securely notify about deployment status in Slack [Issue 194](https://github.com/IntersectMBO/govtool/issues/194).
- Streamlined the application build and deployment process, thereby accelerating continuous delivery (CD) and reducing the resource burden [Issue 246](https://github.com/IntersectMBO/govtool/issues/246).
- Applied unified policy on Docker images tagging [Issue 320](https://github.com/IntersectMBO/govtool/issues/320).
- Reorganised deployment Makefiles in order to better document the process and easier management [Issue 385](https://github.com/IntersectMBO/govtool/issues/385).
- Added a grafana panel to track all the deploys on the target machines [Issue 361](https://github.com/IntersectMBO/govtool/issues/361).
- Check a GA metadata is valid [Issue 535](https://github.com/IntersectMBO/govtool/issues/535)
- Update Cardano Serialization Lib to 12.0.0-alpha.19 [Issue 521](https://github.com/IntersectMBO/govtool/issues/521)
- Extend the eslint config to apply to the style guide of the project [Issue 514](https://github.com/IntersectMBO/govtool/issues/514)
- Update frontend package readme to reflect recent changes [Issue 543](https://github.com/IntersectMBO/govtool/issues/543)
- Change input selection strategy to 3 (random) [Issue 575](https://github.com/IntersectMBO/govtool/issues/575)
- Changed documents to prepare for open source [Issue 737](https://github.com/IntersectMBO/govtool/issues/737)
- Changed copy on maintenance page [Issue 753](https://github.com/IntersectMBO/govtool/issues/753)
- Update link to docs [Issue 1246](https://github.com/IntersectMBO/govtool/issues/1246)
- Change label of Proposal Discussion nav item [Issue 1349](https://github.com/IntersectMBO/govtool/issues/1349)
- Change DRep without metadata name from "Sole Voter" to "Direct Voter" [Issue 880](https://github.com/IntersectMBO/govtool/issues/880)
- Change style of url button to trim the file name [Issue 655](https://github.com/IntersectMBO/govtool/issues/655)
- Change regex for parsing urls to match urls without protocol [Issue 655](https://github.com/IntersectMBO/govtool/issues/655)

### Removed

-

## [sancho-v1.0.2-alpha](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.0-alpha) 2023-04-05

## [sancho-v1.0.0](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.0) 2023-12-17

- Import code to new repository
