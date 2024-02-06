declare namespace Cypress {
  interface Chainable<Subject = any> {
    getBySel(
      selector: string,
      options?: Partial<
        Cypress.Loggable &
          Cypress.Timeoutable &
          Cypress.Withinable &
          Cypress.Shadow
      >
    ): Chainable<Element>;
    runOneTimeWalletSetup(): any;
    getInputByPlaceholder(placeholder: string): Chainable<Element>;
    getButtonByText(text: string): Chainable<Element>;
    setGlobalState(key: string, value: any);
    getGlobalState(key: string): any;
  }
}
