# GovTool Frontend

Installed on your machine:

1. Node.js >= 18 ([official website](https://nodejs.org/en))
2. npm or yarn (recommended) - for package management

Clone the project

```bash
git clone https://github.com/IntersectMBO/govtool
```

Fill .env based on env.example file

Go to the project directory

```bash
cd govtool/frontend
```

Install dependencies

```bash
npm install --force
```

or (recommended)

```bash
yarn install
```

Start the server

```bash
npm run dev
```

or (recommended)

```bash
yarn dev
```

## Developing

### Frontend Built With

1. TypeScript - ^5.0.2
2. React - ^18.2.0
3. React Router Dom - ^6.13.0
4. Vite - ^4.3.9
5. Material UI - ^5.14.4
6. Storybook - ^7.4.5
7. Axios - ^1.4.0
8. React Query - ^3.39.3
9. React-Hook-Form - ^7.47.0
10. Yup - ^1.3.2
11. Keen-Slider - ^6.8.5
12. Sentry - ^7.77.0
13. Cardano serialization lib - 12.0.0-alpha.19
14. i18next - ^23.7.19

### Code Quality and Checks Are Handled BY

1. eslint - ^8.38.0
2. vitest - ^1.1.0
3. chromatic - ^10.0.0

### Prerequisites

Install [`Git`](https://git-scm.com/) - version control.
Recommended [`React developer tools`](https://react.dev/learn/react-developer-tools).

To automatically set correct node version:

1. Install [`nvm`](https://github.com/nvm-sh/nvm)
2. Install `lts/hydrogen` version of node

```bash
  nvm install lts/hydrogen
```

3. Having that every time you enter the `govtool/frontend` package [`nvm`](https://github.com/nvm-sh/nvm) automatically sets the correct version of node.

## To Develop

### Standard way

1. Install modules

```bash
npm install --force
```

or (recommended)

```bash
yarn install
```

2. Launch Server

```bash
npm run dev
```

or (recommended)

```bash
yarn dev
```

#### Using Nix and Direnv

1. Get [Nix](https://nixos.org/download).

2. Get [direnv](https://direnv.net/).

3. Fill .envrc based on envrc.example file in project root.

4. Enter `govtool/frontend` directory:

```sh
cd govtool/frontend
```

5. Allow direnv to setup your environment:

```sh
direnv allow
```

5. Run project

```sh
yarn dev
```

_To update the yarn.lock file after changes are made, run `nix develop .#js` for a developer shell with correct yarn/nodejs versions_

## After development

Check our [Contributing Documentation](../../CONTRIBUTING.md) on how to submit a PR.

### Users

The GovTool application can read and display data from the Cardano chain using REST API.
We distinguish two types of users:

#### without a connected wallet who can:

1. See the governance actions along with their details and the number of votes
<!-- 2. See the list of DReps. -->

#### with connected wallet who can:

1.  See the governance actions along with their details and the number of votes.
2.  Display the wallet status.
3.  Delegate his or her voting power in a form of ADA to dReps.
4.  Register as DRrep or Direct Voter.
5.  Vote for the Governance Actions of his or her choice (if the user is registered).
6.  Create their own Governance Action.
<!-- 7. See the list of DReps from which they can submit their vote. -->
