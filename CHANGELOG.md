# GovTool Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [Unreleased]
- Create Checkbox component. Improve Field and ControlledField [Issue 177](https://github.com/IntersectMBO/govtool/pull/177)
- Vitest unit tests added for utils functions [Issue 81](https://github.com/IntersectMBO/govtool/issues/81)
- i18next library added to FE [Issue 80](https://github.com/IntersectMBO/govtool/issues/80)

### Fixed
- Fix drep voting power calculation [Issue 231](https://github.com/IntersectMBO/govtool/issues/231)
- Fix proposal/list and network/metrics bug that appeared when noone has delegated their funds either to drep_always_abstain or drep_always_no_confidence [Issue 231](https://github.com/IntersectMBO/govtool/issues/231)
- Fix GA details [Issue 272](https://github.com/IntersectMBO/govtool/issues/272)
- Fix copy for maintenance page [Issue 180](https://github.com/IntersectMBO/govtool/issues/180)
- Fix misleading metadata hash text [Issue 90](https://github.com/IntersectMBO/govtool/issues/90)
- Fixed vote calculation problems related to NoConfidence DRep [Issue 59](https://github.com/IntersectMBO/govtool/issues/59)
- Fixed ada-holder/get-current-delegation error when delegated to NoConfidence or AlwaysAbstain dreps. [Issue 82](https://github.com/IntersectMBO/govtool/issues/82)
- Fixed deployment scripts to address [Issue 171](https://github.com/IntersectMBO/govtool/issues/171).

### Changed
- Update Cardano-Serialization-Lib to 12.0.0-alpha.16 [Issue 156](https://github.com/IntersectMBO/govtool/issues/156)
- Changed and improved working conventions docs, PR template and codeowners file, addressing [Issue 88](https://github.com/IntersectMBO/govtool/issues/88).
- Changed Node version from 8.7.1-pre to 8.8.0-pre and DbSync version from sancho-2-3-0 to sancho-4-0-0-fix-config, addressing also [Issue 181](https://github.com/IntersectMBO/govtool/issues/181).
- Reorganized repository to fit new conventions [Issue 85](https://github.com/IntersectMBO/govtool/issues/85).
- Renamed project from VVA to GovTool [Issue 97](https://github.com/IntersectMBO/govtool/issues/97).
- (`docs/update-working-conventions`) Addressing [Issue 25](https://github.com/IntersectMBO/govtool/issues/25) changed working conventions documentation to improve intended flows.
- Adjusted Nix configuration to meet projects needs [Issue 187](https://github.com/IntersectMBO/govtool/issues/187).

### Removed
-

## [sancho-v1.0.0](https://github.com/IntersectMBO/govtool/releases/tag/sancho-v1.0.0) 2023-12-17

- Import code to new repository
