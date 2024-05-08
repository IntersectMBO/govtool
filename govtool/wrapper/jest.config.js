// This configuration for Jest sets up module name mapping, allowing imports with the "@" prefix
// to resolve to the "src" directory.
// It also specifies "jsdom" as the test environment, suitable for DOM-related testing.
// Lastly, it includes a setup file to run custom configurations or imports before each test suite.
module.exports = {
	moduleNameMapper: {
		"^@/(.*)$": "<rootDir>/src/$1",
	},
	testEnvironment: "jsdom",
	setupFilesAfterEnv: ["<rootDir>/jest.setup.js"],
	transform: {
		"^.+\\.(js|jsx|ts|tsx)$": [
			"babel-jest",
			{ configFile: "./babel.config.jest.js" },
		],
	},
};
