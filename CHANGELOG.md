# GovTool Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [Unreleased]

- Add wallet connector package [Issue 898](https://github.com/IntersectMBO/govtool/issues/898)
- Change DRep without metadata name from "Sole Voter" to "Direct Voter" [Issue 880](https://github.com/IntersectMBO/govtool/issues/880)
- Inicialize Usersnap into App [Issue 546](https://github.com/IntersectMBO/govtool/issues/546)
- Integrate frontend with metadata validation service [Issue 617](https://github.com/IntersectMBO/govtool/issues/617)
- Implement a loading modal for the validation of the metadata [Issue 646](https://github.com/IntersectMBO/govtool/issues/646)
- Fix displaying modals to not block signing transactions [Issue 710](https://github.com/IntersectMBO/govtool/issues/710)
- Change style of url button to trim the file name [Issue 655](https://github.com/IntersectMBO/govtool/issues/655)
- Change regex for parsing urls to match urls without protocol [Issue 655](https://github.com/IntersectMBO/govtool/issues/655)
- Integrate ga displaying metadata validation with the validation service [Issue 712](https://github.com/IntersectMBO/govtool/issues/712)
- Correct text of the governance action type [Issue 651](https://github.com/IntersectMBO/govtool/issues/651)
- Enable coverage tests report

### Added

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

### Fixed

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

### Removed

-

## [sancho-v1.0.2-alpha](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.0-alpha) 2023-04-05

## [sancho-v1.0.0](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.0) 2023-12-17

- Import code to new repository
