import { govActionEnums } from "../constants/governanceActions";
import { getShortenedGovActionId } from "./utils";

// Validates that the rendered data on the frontend matches the expected filtered data.
export const validateFilter = (filterText: string) => {
  cy.getBySel(`govaction-${filterText.replace(/ /g, "")}-card`).each(
    ($slide) => {
      cy.wrap($slide).should("contain", filterText);
    }
  );
};

// Validates that the rendered data on the frontend matches the expected sorted order.
export const validateSort = (sortedData: any) => {
  Object.keys(sortedData).forEach((type: string, type_index: number) => {
    cy.wrap(type_index).then(() => {
      cy.getBySel(
        `govaction-${govActionEnums[type].replace(/ /g, "")}-card`
      ).each(($slide, idx) => {
        if (idx > 5) return;

        cy.wrap({ idx, type }).then((context) => {
          cy.wrap($slide)
            .find(
              `[data-testid="${sortedData[context.type][context.idx].txHash}#${
                sortedData[context.type][context.idx].index
              }-id"]`
            )
            .should("have.length.greaterThan", 0);
        });
      });
    });
  });
  cy.contains("Clear", { matchCase: false }).click();
};

export const validateDRepURLInput = (value: string, invalid = true) => {
  cy.getBySel("url-input").type(value).blur();
  if (invalid) {
    cy.getBySel("invalid-url-format-error").should("be.visible");
  } else {
    cy.getBySel("invalid-url-format-error").should("not.exist");
  }

  cy.getBySel("url-input").clear();
};

export const validateDRepMetaHashInput = (value: string, invalid = true) => {
  cy.getBySel("hash-input").type(value).blur();
  if (invalid) {
    cy.getBySel("hash-must-be-exactly-64-characters-long-error").should(
      "be.visible"
    );
  } else {
    cy.getBySel("hash-must-be-exactly-64-characters-long-error").should(
      "not.exist"
    );
  }

  cy.getBySel("hash-input").clear();
};
