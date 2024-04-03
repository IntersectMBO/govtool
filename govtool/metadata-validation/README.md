# GovTool metadata validation tool

Installed on your machine:

1. Node.js >= 18 ([official website](https://nodejs.org/en))
2. yarn - for package management

Clone the project

```bash
git clone https://github.com/IntersectMBO/govtool
```

Fill .env based on env.example file

Go to the project directory

```bash
cd govtool/metadata-validation
```

Install dependencies

```bash
yarn install
```

Start the server

```bash
yarn start:dev
```

## Developing

### Built With

1. @nestjs - ^10.0.0
2. axios - ^1.6.8
3. blakejs - ^1.2.1
4. jsonld - ^8.3.2
5. rxjs - ^7.8.1
6. typescript - ^5.1.3

### Code Quality and Checks Are Handled By

1. eslint - ^8.42.0
2. prettier - ^3.0.0
3. jest - ^29.5.0

### Prerequisites

Install [`Git`](https://git-scm.com/) - version control.
Recommended [`React developer tools`](https://react.dev/learn/react-developer-tools).

To automatically set correct node version:

1. Install [`nvm`](https://github.com/nvm-sh/nvm)
2. Install `lts/hydrogen` version of node

```bash
  nvm install lts/hydrogen
```

3. Install [@nestjs/cli](https://docs.nestjs.com/cli/overview) globally

```bash
npm install -g @nestjs/cli
```

## To Develop

### Standard way

1. Install modules

```bash
yarn install
```

2. Launch Server

```bash
yarn start:dev
```

#### Using Docker

1. Get [Docker](https://www.docker.com/).

2. Build the image

```bash
docker build -t metadata-validation .
```

3. Fill .env based on env.example file

4. Run the container

```bash
docker run -p 3000:3000 -d metadata-validation
```

## After development

Check our [Contributing Documentation](../../CONTRIBUTING.md) on how to submit a PR.
