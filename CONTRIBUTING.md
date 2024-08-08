# Contributing to the `GovTool` project

⚠️ This is a work in progress document, more instruction on how-to contribute to come!

Contributing todo:

- [ ] Align with latest OSC policies
- [ ] Refactor to reflect reality
- [ ] Make more friendly to open source contributors

Thanks for considering contributing and helping us on creating GovTool! 😎

The best way to contribute right now is to try things out and provide feedback, but we also accept contributions to the documentation and the obviously to the code itself.

This document contains guidelines to help you get started and how to make sure your contribution gets accepted, making you our newest GovTool contributor!

## Table of Contents

- [Contributing to the `GovTool` project](#contributing-to-the-govtool-project)
  - [Table of Contents](#table-of-contents)
  - [Code of Conduct](#code-of-conduct)
  - [Ask for Help](#ask-for-help)
  - [Roles and Responsibilities](#roles-and-responsibilities)
  - [I Want To Contribute](#i-want-to-contribute)
    - [Before Submitting a Bug Report](#before-submitting-a-bug-report)
    - [How Do I Submit a Good Bug Report?](#how-do-i-submit-a-good-bug-report)
    - [Your First Code Contribution](#your-first-code-contribution)
  - [Working Conventions](#working-conventions)
    - [Pull Requests](#pull-requests)
    - [Branch Naming](#branch-naming)
    - [Commit Messages](#commit-messages)
      - [Rationale](#rationale)
    - [Merge Commit PRs and Rebase Branches on top of Main](#merge-commit-prs-and-rebase-branches-on-top-of-main)
      - [Rationale](#rationale-1)
    - [Versioning](#versioning)
    - [Changelog](#changelog)
    - [Style Guides](#style-guides)
      - [React](#react)
      - [CSS in Javascript](#css-in-javascript)
      - [CSS / SASS](#css--sass)
      - [Haskell](#haskell)
  - [Development Processes](#development-processes)
    - [Developer workflow](#developer-workflow)
    - [QA Workflow](#qa-workflow)
    - [PO Workflow](#po-workflow)
    - [Tech Lead Workflow](#tech-lead-workflow)

## Code of Conduct

This project and everyone participating in it is governed by the [Code of Conduct](./CODE-OF-CONDUCT.md).
By participating, you are expected to uphold this code.

## Ask for Help

See [`SUPPORT.md`](./SUPPORT.md) should you have any questions or need some help in getting set up.

## Roles and Responsibilities

We maintain a [CODEOWNERS file](./CODEOWNERS) which provides information who should review a contributing PR.
Note that you might need to get approvals from all code owners (even though GitHub doesn't give a way to enforce it).

## I Want To Contribute

#### Before Submitting a Bug Report

A good bug report shouldn't leave others needing to chase you up for more information.
Therefore, we ask you to investigate carefully, collect information and describe the issue in detail in your report.
Please complete the following steps in advance to help us fix any potential bug as fast as possible.

- Make sure that you are using the latest version.
- Determine if your bug is really a bug and not an error on your side.
  e.g. using incompatible environment components/versions.
  If you are looking for support, you might want to check [this section](#i-have-a-question).
- To see if other users have experienced (and potentially already solved) the same issue you are having.
- Also make sure to search the internet (including Stack Overflow)
  to see if users outside of the GitHub community have discussed the issue.
- Collect information about the bug:
  - Stack trace (Traceback)
  - OS, Platform and Version (Windows, Linux, macOS, x86, ARM)
  - Version of the interpreter, compiler, SDK, runtime environment, package manager, depending on what seems relevant.
  - Possibly your input and the output
  - Can you reliably reproduce the issue? And can you also reproduce it with older versions?

#### How Do I Submit a Good Bug Report?

We use GitHub issues to track bugs and errors. If you run into an issue with the project:

- Open an [Issue](https://github.com/IntersectMBO/govtool/issues/new).
  (Since we can't be sure at this point whether it is a bug or not, we ask you not to talk about a bug yet and not to label the issue.)
- Explain the behavior you would expect and the actual behavior.
- Please provide as much context as possible.
  Describe the _reproduction steps_ that someone else can follow to recreate the issue on their own.
  This usually includes your code.
  For good bug reports you should isolate the problem and create a reduced test case.
- Provide the information you collected in the previous section.

Once it's filed:

- The project team will label the issue accordingly.
- A team member will try to reproduce the issue with your provided steps.
  If there are no reproduction steps or no obvious way to reproduce the issue, the team will ask you for those steps.
  The issue would then be marked as `needs-repro`.
  Bugs with the `needs-repro` tag will not be addressed until they are reproduced.
- If the team is able to reproduce the issue, it will be marked `needs-fix`.
  It may possibly be marked with other tags (such as `critical`).
  The issue will then be left to be [implemented by someone](#your-first-code-contribution).

#### Your First Code Contribution

TODO

## Working Conventions

### Pull Requests

Thank you for contributing your changes by opening a pull requests!

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
- Using prefixes such as `feat/`, `fix/`, `chore/`, `docs/` for branch names are a good start.
- Using the related issue number after the prefix is required.

Examples:

- `feat/123-added-ability-for-dreps-to-change-drep-id`
- `fix/312-fixed-drep-ids-being-reversed`
- `chore/567-bumped-cardano-node-version-to-9`
- `docs/88-tweak-contributing-pr-template-codeowners`

### Commit Messages

Please make informative commit messages!
It makes it much easier to work out why things are the way they are when you’re debugging things later.

A commit message is communication, so as usual, put yourself in the position of the reader: what does a reviewer, or someone reading the commit message later need to do their job?
Write it down!
It is even better to include this information in the code itself, but sometimes it doesn’t belong there (e.g. ticket info).

Also, include any relevant meta-information, such as issue numbers.
If a commit completely addresses a issue, you can put that in the headline if you want, but it’s fine to just put it in the body.

Here are seven rules for great git commit messages:

1. Separate subject from body with a blank line
2. Limit the subject line to 50 characters (soft limit)
3. Capitalize the subject line
4. Do not end the subject line with a period
5. Use the imperative mood in the subject line and suffix with ticket number if applicable
6. Wrap the body at 72 characters (hard limit)
7. Use the body to explain what and why vs. how

There is plenty to say on this topic, but broadly the guidelines in [this post](https://cbea.ms/git-commit/) are good.

#### Rationale

Git commit messages are our only source of why something was changed the way it was changed.
So we better make the readable, concise and detailed (when required).

### Merge Commit PRs and Rebase Branches on top of Main

When closing branches / PRs use merge commits, so we have a history of PRs also in the git history.
Do not merge main into side branches, instead rebase them on top of main.
Try to keep branches up-to-date with main (not strict requirement though).
Once merged to main, please delete the branch.

**Tip:** Use Github's merge button in PRs to merge with commit.
This strategy helps us operate on the commits you've delivered: it's easier to [cherry-pick a merge commit](https://git-scm.com/docs/git-cherry-pick#Documentation/git-cherry-pick.txt--mltparent-numbergt) than a series of commits, and it's also easier to [revert changes using a merge commit](https://git-scm.com/docs/git-revert#Documentation/git-revert.txt--mparent-number) instead of a series of reverts.
If a branch is outdated, use the rebase button in PRs to rebase feature branches (NOT update via merge).

#### Rationale

Keeping branches ahead of main not only make the git history a lot nicer to process, it also makes conflict resolutions easier.
Merging main into a branch repeatedly is a good recipe to introduce invalid conflict resolutions and loose track of the actual changes brought by a the branch.

### Versioning

Not all releases are declared stable.
Releases that aren't stable will be released as pre-releases and will append a -pre tag indicating it is not ready for running on production networks.

### Changelog

During development, on every PR;

- Make sure `CHANGELOG.md` is kept up-to-date with high-level, technical, but user-focused list of changes according to [keepachangelog](https://keepachangelog.com/en/1.0.0/).
- Bump `UNRELEASED` version in `CHANGELOG.md` according to [semver](https://semver.org/).

### Style Guides

#### React

Please see [React Style Guide](./docs/style-guides/react/).

#### CSS in Javascript

Please see [CSS in Javascript Style Guide](./docs/style-guides/css-in-js/).

#### CSS / SASS

Please see [CSS / SASS Style Guide](./docs/style-guides/css-sass/).

#### Haskell

Please see [stylish-haskell configuration](./govtool/backend/.stylish-haskell.yaml).

## Development Processes

### Developer workflow

- Choose ticket/issue to work on from the project, move ticket from `todo` to `in progress`.
- Create [well named](#branch-naming) branch from `develop` add changes, then make a pull request back to the `develop` branch.
- If the changes are not ready for review then feel free to create a draft PR, and link this to the ticket/issue.
- When the PR is ready for review move the ticket from `in progress` to `in review`. Remember to change the state of the PR from draft to actual PR.
- Developers should review each other's pull requests, and should be requested via [CODEOWNERS](./CODEOWNERS).
- Unit tests are run on each pull request to `develop`.
- After a review remember to address all the requests of changes since they are blocking PR from being merged.
- Once tests pass and peer review is done the branch can be merged into `develop` by author and then deployed to the dev environment (manually).
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
