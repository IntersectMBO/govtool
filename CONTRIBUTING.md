# Contributing to the `GovTool` project

Thanks for considering contributing and helping us on creating GovTool! 😎

The best way to contribute right now is to try things out and provide feedback, but we
also accept contributions to the documentation and obviously to the code itself.

This document gets you from "I want to help" to a merged pull request.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Ask for Help](#ask-for-help)
- [Ways to Contribute](#ways-to-contribute)
- [Reporting Bugs](#reporting-bugs)
  - [Before Submitting a Bug Report](#before-submitting-a-bug-report)
  - [How Do I Submit a Good Bug Report?](#how-do-i-submit-a-good-bug-report)
- [Your First Code Contribution](#your-first-code-contribution)
  - [1. Set up your environment](#1-set-up-your-environment)
  - [2. Find an issue to work on](#2-find-an-issue-to-work-on)
  - [3. Claim the issue](#3-claim-the-issue)
  - [4. Fork the repository, if needed](#4-fork-the-repository-if-needed)
  - [5. Create a branch](#5-create-a-branch)
  - [6. Make your changes](#6-make-your-changes)
  - [7. Run the checks](#7-run-the-checks)
  - [8. Commit your changes](#8-commit-your-changes)
  - [9. Update the changelog](#9-update-the-changelog)
  - [10. Open a pull request](#10-open-a-pull-request)
  - [11. Address feedback](#11-address-feedback)
- [Working Conventions](#working-conventions)
  - [Branches and Environments](#branches-and-environments)
  - [Pull Requests](#pull-requests)
  - [Branch Naming](#branch-naming)
  - [Commit Messages](#commit-messages)
  - [Merge Commit PRs and Rebase Branches on top of the Base Branch](#merge-commit-prs-and-rebase-branches-on-top-of-the-base-branch)
  - [Changelog](#changelog)
  - [Versioning](#versioning)
  - [Style Guides](#style-guides)
- [Roles and Responsibilities](#roles-and-responsibilities)
- [Development Processes](#development-processes)
  - [Developer workflow](#developer-workflow)
  - [QA Workflow](#qa-workflow)
  - [PO Workflow](#po-workflow)
  - [Tech Lead Workflow](#tech-lead-workflow)

## Code of Conduct

This project and everyone participating in it is governed by the [Code of Conduct](./CODE-OF-CONDUCT.md).
By participating, you are expected to uphold this code.

## Ask for Help

See [`SUPPORT.md`](./SUPPORT.md) should you have any questions or need some help in
getting set up. Questions are best asked in
[GitHub Discussions](https://github.com/IntersectMBO/govtool/discussions), where others
can benefit from the answer too.

Security vulnerabilities are different: do not open an issue, follow
[`SECURITY.md`](./SECURITY.md) instead.

## Ways to Contribute

- Use GovTool on [mainnet](https://gov.tools/) or [preview](https://preview.gov.tools/) and report what does not work.
- Improve documentation, in this repository or on [docs.gov.tools](https://docs.gov.tools/).
- Fix a bug or implement a feature idea, see [Your First Code Contribution](#your-first-code-contribution).

## Reporting Bugs

### Before Submitting a Bug Report

A good bug report shouldn't leave others needing to chase you up for more information.
Therefore, we ask you to investigate carefully, collect information and describe the
issue in detail in your report.
Please complete the following steps in advance to help us fix any potential bug as fast
as possible.

- Make sure that you are using the latest version.
- Determine if your bug is really a bug and not an error on your side,
  e.g. using incompatible environment components/versions.
  If you are looking for support, see [Ask for Help](#ask-for-help).
- Search the [open issues](https://github.com/IntersectMBO/govtool/issues) to see if
  other users have experienced (and potentially already solved) the same issue.
- Also make sure to search the internet (including Stack Overflow)
  to see if users outside of the GitHub community have discussed the issue.
- Collect information about the bug:
  - Stack trace (Traceback)
  - OS, Platform and Version (Windows, Linux, macOS, x86, ARM)
  - Browser and wallet extension, with versions, for frontend issues
  - Version of the interpreter, compiler, SDK, runtime environment, package manager, depending on what seems relevant.
  - Possibly your input and the output
  - Can you reliably reproduce the issue? And can you also reproduce it with older versions?

### How Do I Submit a Good Bug Report?

We use GitHub issues to track bugs and errors. If you run into an issue with the project:

- Open an [Issue](https://github.com/IntersectMBO/govtool/issues/new/choose) using the
  🐛 Bug report template.
  (Since we can't be sure at this point whether it is a bug or not, we ask you not to
  talk about a bug yet and not to label the issue.)
- Explain the behavior you would expect and the actual behavior.
- Please provide as much context as possible.
  Describe the _reproduction steps_ that someone else can follow to recreate the issue
  on their own.
  This usually includes your code.
  For good bug reports you should isolate the problem and create a reduced test case.
- Provide the information you collected in the previous section.

Once it's filed:

- The project team will label the issue accordingly.
- A team member will try to reproduce the issue with your provided steps.
  If there are no reproduction steps or no obvious way to reproduce the issue, the team
  will ask you for those steps.
  The issue would then be marked as `needs-repro`.
  Bugs with the `needs-repro` tag will not be addressed until they are reproduced.
- If the team is able to reproduce the issue, it will be marked `needs-fix`.
  It may possibly be marked with other tags (such as `critical`).
  The issue will then be left to be [implemented by someone](#your-first-code-contribution).

## Your First Code Contribution

Welcome! Whether you're fixing a bug, adding a feature, or improving documentation,
we're excited to have you on board. The steps below take you through one full
contribution.

### 1. Set up your environment

Follow [Getting started](./README.md#-getting-started) in the README and set up only the
package you intend to change:

- UI work: [`govtool/frontend`](./govtool/frontend/README.md), Node is pinned by
  `.nvmrc` and you can run against a deployed backend, so no local chain data is needed.
- API work: [`govtool/backend`](./govtool/backend/README.md), needs access to a
  `cardano-db-sync` Postgres database.
- Metadata rules: [`govtool/metadata-validation`](./govtool/metadata-validation/README.md).
- Everything at once: [`docker/`](./docker/README.md).

Make sure you're working from the latest `develop` to avoid potential conflicts.

### 2. Find an issue to work on

- Browse open issues on the [GovTool GitHub Issues page](https://github.com/IntersectMBO/govtool/issues).
- Look for issues labeled [`🐛 Bug`](https://github.com/IntersectMBO/govtool/issues?q=is%3Aissue+is%3Aopen+label%3A%22%F0%9F%90%9B+Bug%22)
  or [`💡 Feature idea`](https://github.com/IntersectMBO/govtool/issues?q=is%3Aissue+is%3Aopen+label%3A%22%F0%9F%92%A1+Feature+idea%22).
- For existing feature idea tasks, comment on the issue to express interest or share thoughts.
- No issue for what you want to change? Open one first, so the approach can be agreed
  before you write code.

### 3. Claim the issue

- Comment on the issue to let maintainers know you're working on it. This helps avoid
  duplicate efforts.
- Move the issue from `todo` to `in progress` on the project board (if you have permissions).

### 4. Fork the repository, if needed

This step applies only if you lack permission to create branches in the `govtool`
repository. Fork it on GitHub to create a copy under your account.

### 5. Create a branch

Create a new branch from `develop`, never from `main`, with a descriptive name
(see [Branch Naming](#branch-naming)).

```bash
git checkout develop
git pull
git checkout -b feat/123-add-voting-ui
```

### 6. Make your changes

- Write clean, well-documented code following the [Style Guides](#style-guides) for
  React, Haskell, CSS, or other relevant technologies.
- Match the conventions of the files around you.
- Add or update tests to ensure your changes are robust.
- Keep your changes focused and aligned with the issue's scope.

### 7. Run the checks

Run the checks for the package you touched before pushing, CI runs the same ones.

```bash
# govtool/frontend
npm run lint && npm run tsc && npx vitest run   # note: npm run test is watch mode

# govtool/metadata-validation
npm run lint && npm test

# govtool/backend (inside the nix shell)
cabal build all && pre-commit run --all-files hlint && pre-commit run --all-files stylish-haskell
```

### 8. Commit your changes

Write clear, concise commit messages following the [Commit Messages](#commit-messages)
guidelines, and reference the issue number.

### 9. Update the changelog

Add a line to [`CHANGELOG.md`](./CHANGELOG.md) under `[Unreleased]`, in the matching
`Added` / `Fixed` / `Changed` / `Removed` subsection, with a link to the issue. See
[Changelog](#changelog).

### 10. Open a pull request

- If you have push permissions: push your branch to this repository and open a pull
  request against `develop`.
- If you work from a fork: push your branch to your fork and open a pull request in the
  `IntersectMBO/govtool` repository, with `develop` as the base branch and your fork's
  branch as the head.
- Fill in the PR template and link the related issue (e.g. `issues #123`).
- Describe your changes clearly, including why they're needed and how they were tested.
- If your PR isn't ready for review, open it as a draft.

### 11. Address feedback

- Expect reviews from maintainers or other contributors (see [CODEOWNERS](./CODEOWNERS)).
- Respond to feedback promptly and make requested changes.
- Once approved and green, the PR is merged into `develop` by the author.

🎉 Once merged, your changes move through the [Development Processes](#development-processes)
(QA, staging, release). You're now a `GovTool` contributor, consider tackling another issue!

## Working Conventions

### Branches and Environments

Changes flow in one direction through four long-lived branches:

`develop` → `test` → `staging` → `main`

- `develop` is where all contributions land, and it is the base branch for every PR.
- `test` and `staging` are promotion branches driven by QA and the tech lead.
- `main` tracks what has been released, never branch off it and never PR into it directly.

### Pull Requests

Thank you for contributing your changes by opening a pull request!

To get something merged we usually require:

- Follow the Pull Request template
- Description of the changes - if your commit messages are great, this is less important
- Quality of changes is ensured - through new or updated automated tests
- Change is related to an issue, feature (idea) or bug report - ideally discussed beforehand
- Well-scoped - we prefer multiple PRs, rather than a big one
- Add changes to changelog, see [Changelog](#changelog).

Please reuse the branch naming for the pull request naming.

### Branch Naming

- When creating your branches please create informative names.
- Use  prefixes such as `feat/`, `fix/`, `chore/`, `docs/`.
- Using the related issue number after the prefix is required.

Examples:

- `feat/123-added-ability-for-dreps-to-change-drep-id`
- `fix/312-fixed-drep-ids-being-reversed`
- `chore/567-bumped-cardano-node-version-to-9`
- `docs/88-tweak-contributing-pr-template-codeowners`

### Commit Messages

Please make informative commit messages!
It makes it much easier to work out why things are the way they are when you're
debugging things later.

A commit message is communication, so as usual, put yourself in the position of the
reader: what does a reviewer, or someone reading the commit message later need to do their job?
Write it down!
It is even better to include this information in the code itself, but sometimes it
doesn't belong there (e.g. ticket info).

Also, include any relevant meta-information, such as issue numbers.
If a commit completely addresses an issue, you can put that in the headline if you want,
but it's fine to just put it in the body.

Here are seven rules for great git commit messages:

1. Separate subject from body with a blank line
2. Limit the subject line to 50 characters (soft limit)
3. Capitalize the subject line
4. Do not end the subject line with a period
5. Use the imperative mood in the subject line and suffix with ticket number if applicable
6. Wrap the body at 72 characters (hard limit)
7. Use the body to explain what and why vs. how

There is plenty to say on this topic, but broadly the guidelines in
[this post](https://cbea.ms/git-commit/) are good.

**Rationale:** git commit messages are our only source of why something was changed the
way it was changed. So we better make them readable, concise and detailed (when required).

### Merge Commit PRs and Rebase Branches on top of the Base Branch

When closing branches / PRs use merge commits, so we have a history of PRs also in the
git history.
Do not merge the base branch (usually `develop`) into side branches, instead rebase them
on top of it.
Try to keep branches up-to-date with the base branch (not a strict requirement though).
Once merged, please delete the branch.

**Tip:** Use GitHub's merge button in PRs to merge with commit.
This strategy helps us operate on the commits you've delivered: it's easier to
[cherry-pick a merge commit](https://git-scm.com/docs/git-cherry-pick#Documentation/git-cherry-pick.txt--mltparent-numbergt)
than a series of commits, and it's also easier to
[revert changes using a merge commit](https://git-scm.com/docs/git-revert#Documentation/git-revert.txt--mparent-number)
instead of a series of reverts.
If a branch is outdated, use the rebase button in PRs to rebase feature branches (NOT
update via merge).

**Rationale:** keeping branches ahead of the base branch not only makes the git history
a lot nicer to process, it also makes conflict resolutions easier.
Merging the base branch into a branch repeatedly is a good recipe to introduce invalid
conflict resolutions and lose track of the actual changes brought by the branch.

### Changelog

On every PR, keep [`CHANGELOG.md`](./CHANGELOG.md) up-to-date with a high-level,
technical, but user-focused list of changes, according to
[keepachangelog](https://keepachangelog.com/en/1.0.0/).

Add your line under `[Unreleased]`, in the `Added`, `Fixed`, `Changed` or `Removed`
subsection, and link the issue:

```markdown
### Fixed

- Fix disappearing proposals in the governance actions list for the same tx hashes [Issue 3918](https://github.com/IntersectMBO/govtool/issues/3918)
```

### Versioning

Do not hand-bump version numbers. `package.json`, `vva-be.cabal` and the Dockerfiles are
bumped together by the `Update GovTool Version and Changelog` workflow on manual
dispatch, which also turns the `[Unreleased]` changelog section into a released one.

Releases follow [semver](https://semver.org/). Not all releases are declared stable:
releases that aren't stable are published as pre-releases and append a `-pre` tag
indicating they are not ready for running on production networks.

### Style Guides

- React: [React Style Guide](./docs/style-guides/react/)
- CSS in JavaScript: [CSS in Javascript Style Guide](./docs/style-guides/css-in-js/)
- CSS / SASS: [CSS / SASS Style Guide](./docs/style-guides/css-sass/)
- Haskell: [stylish-haskell configuration](./govtool/backend/.stylish-haskell.yaml) plus `hlint`

## Roles and Responsibilities

We maintain a [CODEOWNERS file](./CODEOWNERS) which provides information who should
review a contributing PR.
Note that you might need to get approvals from all code owners (even though GitHub
doesn't give a way to enforce it).

## Development Processes

These describe how the core team moves a ticket from idea to release. As an external
contributor you are only involved in the developer workflow.

### Developer workflow

- Choose ticket/issue to work on from the project, move ticket from `todo` to `in progress`.
- Create [well named](#branch-naming) branch from `develop`, add changes, then make a pull request back to the `develop` branch.
- If the changes are not ready for review then feel free to create a draft PR, and link this to the ticket/issue.
- When the PR is ready for review move the ticket from `in progress` to `in review`. Remember to change the state of the PR from draft to actual PR.
- Developers should review each other's pull requests, and should be requested via [CODEOWNERS](./CODEOWNERS).
- Unit tests are run on each pull request to `develop`.
- After a review remember to address all the requests of changes since they are blocking the PR from being merged.
- Once tests pass and peer review is done the branch can be merged into `develop` by the author and then deployed to the dev environment (manually).
- The ticket status can then be moved to `in QA` making sure that the PR/branch has been added to the ticket/issue as a comment.

### QA Workflow

- Choose ticket from `in QA`.
- Merge in the ticket's changes from `develop` branch into `test` branch.
- Deploy to test environment is performed automatically once the ticket is merged to the `test` branch.
- The QA tests the deployed test environment against the ticket.
- If QA agrees that the code is good, they can make a PR from `test` branch to `staging` branch where end-to-end and performance tests are run.
- If tests pass, then QA or tech lead can merge and deploy to staging environment (automatically).
- Moving ticket to `staging` status this ready for PO check.

### PO Workflow

- Choose ticket from `staging` status.
- Compare the deployment on staging environment to the contents of the ticket.
- If the deployment has been satisfied via the staging environment, PO comments on the ticket to be included in next release.

### Tech Lead Workflow

- Bundle the staging status tickets together into a new tag.
- Merge `staging` branch into the `main` branch.
- Deploy tagged build to `beta` environment.
- Move tickets from staging status to done status.

---

This document is still evolving. Known gaps we intend to close:

- [ ] Align with latest OSC policies
- [ ] Keep the development processes in sync with how the team actually works
