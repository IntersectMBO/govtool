import * as addCommands from "cypress-browser-extension-plugin/commands";

Cypress.Commands.add("getBySel", (selector, ...args): void | any => {
  return cy.get(`[data-testid="${selector}"]`, ...args);
});

Cypress.Commands.add("getInputByPlaceholder", (placeholder): void | any => {
  return cy.get(`input[placeholder*="${placeholder}"]`);
});

Cypress.Commands.add("getButtonByText", (text): void | any => {
  return cy.get("button").filter(`:contains("${text}")`);
});

Cypress.Commands.add("setGlobalState", (key, value) => {
  cy.window().then((w: any) => {
    w.myGlobalState = w.myGlobalState || {};
    w.myGlobalState[key] = value;
  });
});

Cypress.Commands.add("getGlobalState", (key) => {
  return cy.window().then((w: any) => {
    return w.myGlobalState && w.myGlobalState[key];
  });
});

addCommands(Cypress);
