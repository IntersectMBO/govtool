# GovTool Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [Unreleased]
- Create GA review subbmision page [Issue 362](https://github.com/IntersectMBO/govtool/issues/362)
- Create GA creation form [Issue 360](https://github.com/IntersectMBO/govtool/issues/360)
- Create TextArea [Issue 110](https://github.com/IntersectMBO/govtool/issues/110)
- Abandoning registration as DRep [Issue 151](https://github.com/IntersectMBO/govtool/issues/151)
- Abandoning GA creation [Issue 359](https://github.com/IntersectMBO/govtool/issues/359)
- Choose GA type - GA Submiter [Issue 358](https://github.com/IntersectMBO/govtool/issues/358)
- Change step 3 components [Issue 152](https://github.com/intersectMBO/govtool/issues/152)
- Add possibility to vote on behalf of myself - Sole Voter [Issue 119](https://github.com/IntersectMBO/govtool/issues/119)
- Create DRep registration page about roles [Issue 205](https://github.com/IntersectMBO/govtool/issues/205)
- Create Checkbox component. Improve Field and ControlledField [Issue 177](https://github.com/IntersectMBO/govtool/pull/177)
- Vitest unit tests added for utils functions [Issue 81](https://github.com/IntersectMBO/govtool/issues/81)
- i18next library added to FE [Issue 80](https://github.com/IntersectMBO/govtool/issues/80)

### Added
- Added `isRegisteredAsSoleVoter` and `wasRegisteredAsSoleVoter` fields to the drep/info response [Issue 212](https://github.com/IntersectMBO/govtool/issues/212)

### Fixed
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

### Changed
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

### Removed
-

## [sancho-v1.0.0](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.0) 2023-12-17

- Import code to new repository
