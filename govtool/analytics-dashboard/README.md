# 🚀 Frontend setup

## Table of content:

- [Introduction](#introduction)
- [Prerequisites](#prerequisites)
- [Running locally](#running-locally)
- [Testing](#testing)
- [Running using docker compose](#running-using-docker-compose)
- [Directory structure](#directory-structure)

## Introduction

This document contains instructions for setting up and running the frontend part of the application. The frontend is developed using [Next.js](https://nextjs.org/).

## Prerequisites

Before starting, please ensure you have the following:

- Node.js and npm - Latest versions. You can download them from [here](https://nodejs.org/en/download/).
- Docker and Docker Compose installed (for Docker environment) - [Download link](https://docs.docker.com/get-started/)

## Running locally

Follow these steps to set up and run your Next.js project in development mode on your local machine. Development mode provides features like hot code reloading and detailed error reporting that are invaluable during development.

1. **Navigate to the frontend directory:**

   - Open a terminal and change your current directory to the frontend part of your application using `cd frontend`.

2. **Install dependencies:**

   - Next.js projects have dependencies that need to be installed before you can start development. Use npm or yarn to install these dependencies: `npm install` or if you prefer yarn: `yarn install`. This command reads the `package.json` file in your project directory and installs all the required packages listed under dependencies and devDependencies.

3. **Configure environment variables:**

   - Create a `.env` file in the root of your frontend directory. This file should contain all the necessary environment configurations for your project. Use the `.env.example` as a template if your project includes one.

   - `NEXT_PUBLIC_` **Prefix**: In Next.js, any environment variable that should be exposed to the browser side of the application must be prefixed with NEXT*PUBLIC*. This prefix tells Next.js to include the variable in the JavaScript bundle sent to the browser. Without this prefix, environment variables are only available in the Node.js environment, meaning they can't be accessed from client-side code.

   - For this Next.js project, you should at least configure the following variable:
     - `NEXT_PUBLIC_API_URL`: This is used in Next.js applications to specify the base URL of the API that the frontend application should communicate with

4. **Start the frontend in development mode:**

   - With the dependencies installed, you can now start the Next.js application in development mode. Development mode enables features like hot reloading, which automatically updates your application as you make changes to the code: `npm run dev` or with yarn `yarn dev`
   - This command starts the Next.js development server. You'll see output in the terminal indicating the server is running, typically along with the URL where you can view your application, usually `http://localhost:3000`.
   - Once the development server is running, open a web browser and go to `http://localhost:3000`. You should see your Next.js application running. Any changes you make to your React components will automatically refresh the page.

5. **Start the frontend in production mode (Optional):**

   - If you need to simulate a production environment on your local machine, you can build and start your Next.js application in production mode. This mode includes optimizations like minification, asset optimization, server-side rendering, and static site generation, depending on your application's configuration. Running in production mode is an excellent way to test the performance and behavior of your application before deploying it.
   - Building the Application for Production:
     - First, build your Next.js application by running: `npm run build` or if you prefer yarn then `yarn build`. This command prepares your application for production by compiling and optimizing it. The output is stored in the `.next` directory, containing the production build artifacts.
     - Starting the Application in Production Mode: Once the build is complete, you can serve your application in production mode using npm command `npm start` or if you prefer yarn `yarn start`.
     - This step is optional and primarily for pre-deployment testing. For actual deployments, consider using a hosting solution or server environment tailored for Next.js applications.

## Testing

For maintaining high code quality and ensuring that new changes do not break existing functionality, our Next.js project uses Jest as the testing framework.

### Test Structure

The tests are organized within the `src/tests` directory and are structured to reflect the components and utilities of the project:

- `components`: Contains Jest test files for Next.js components. Each component has a corresponding `.test.js` file that contains tests specific to that component's functionality.

- `utils`: Includes Jest test files for utility functions used throughout the application. Each utility function has a corresponding `.test.js` file.

### Running Tests

To run the tests, we have a script defined in `package.json`. You can execute the tests by running the following command in your terminal: `npm run test`

## Running using docker compose

Docker Compose facilitates the running of multi-container Docker applications. By using a `docker-compose.yaml` file, you can configure your application’s services, networks, and volumes in a single file, then start all services with a single command.

### Prerequisites:

- Docker and Docker Compose installed on your machine.
- Basic understanding of Docker and Docker Compose concepts.
- Familiarity with Next.js and its environment setup.

### Steps to run

1. **Locate the `docker-compose.yaml` File**

   - Ensure you are in the project's root directory, where your `docker-compose.yaml` file is located. This file is crucial for setting up your entire application environment, including the frontend Next.js service alongside Strapi backend service and PostgreSQL database. By defining these services, the `docker-compose.yaml` file facilitates an integrated environment where each component of the application—backend, database, and frontend—can operate cohesively

2. **Review and update the docker compose configuration**

   - Open your `docker-compose.yaml` file to review the configurations for the Next.js frontend service. The most crucial part for the frontend service configuration is the environment variable `NEXT_PUBLIC_API_URL`. This variable enables your Next.js application to communicate with the Strapi backend service.

3. **Environment variables:**

   - For a Next.js application, environment variables can be crucial for defining API endpoints or other runtime configurations.

   - Specify any necessary environment variables within the Docker Compose file under the environment section for the frontend service or through an `.env` file referenced in the compose file. This includes any API URLs or keys that your Next.js application requires.

4. **Starting the services**

   - From the terminal, run `docker-compose up` to start all services defined in the `docker-compose.yaml` file. If you prefer to run the services in the background, use `docker-compose up -d`.

5. **Accessing the Next.js application**

   - With the services up and running, you can access your Next.js application by navigating to `http://localhost:3000` (or another port if you've configured it differently) in your web browser. You'll see your Next.js frontend, now running in a containerized environment, possibly communicating with your backend API if configured.

### Shutting down services:

- To stop and remove all the running services, use `docker-compose down`. If you want to preserve the data in your volumes, make sure not to use the `-v` flag with this command.

By following these steps, you create a cohesive development environment where your Next.js frontend, Strapi backend, and PostgreSQL database (or any other services your application depends on) are all running in a controlled, Dockerized setting. This ensures consistency across development, testing, and production environments and simplifies the process of managing multi-component applications.

## Directory structure

The `frontend` folder of a Next.js project using App Router contains several important directories and files that structure the application. Here is an overview of some of the key components:

- **`messages/`**: Contains localization files, such as `en.json`, `de.json`, etc, for internationalization.
- **`public/`**: Holds static files, like images
- **`src/`**: The source directory, with:
  - **`app/`**: For Next.js pages and possibly layout components.
    - **`[locale]/`**: This is crucial for supporting multiple languages and locale switching in your Next.js project. This structure, aligns with Next.js's internationalized routing capabilities, allowing you to create localized versions of your pages and layouts. It's a great setup for projects aiming to provide a tailored experience across different regions, utilizing Next.js's built-in support for internationalization.
  - **`components/`**, **`constants/`**, **`context/`**, **`lib/`**: For reusable components, constants, context providers, and library functions respectively.
  - **`middleware.js`**, **`navigation.js`**, **`i18n.js`**: For application middleware, navigation, and internationalization setup.
- **`.env`**: An environment variables file where you can store sensitive or environment-specific settings, such as API URLs.

- **`node_modules/`**: Houses all the npm packages and dependencies your project needs, installed based on the `package.json` file.

- **`package.json`**: Defines the project dependencies, scripts, and general metadata about the project. This includes scripts for starting the application, installing dependencies, and other command-line tasks.

```

```
