import { faker } from "@faker-js/faker";

export const invalid = {
  url: () => {
    const invalidSchemes = ["ftp", "unsupported", "unknown-scheme"];
    const invalidCharacters = "<>@!#$%^&*()";
    const invalidTlds = [".invalid", ".example", ".test"];

    const scheme =
      invalidSchemes[Math.floor(Math.random() * invalidSchemes.length)];
    const invalidChar =
      invalidCharacters[Math.floor(Math.random() * invalidCharacters.length)];
    const invalidTld =
      invalidTlds[Math.floor(Math.random() * invalidTlds.length)];

    const randomDomain = `example${invalidChar}domain${invalidTld}`;
    return `${scheme}://${randomDomain}`;
  },

  name: () => {
    const choice = faker.number.int({ min: 1, max: 3 });
    if (choice === 1) {
      // space invalid
      return faker.lorem.word() + " " + faker.lorem.word();
    } else if (choice === 2) {
      // maximum 80 words invalid
      return faker.lorem.paragraphs().replace(/\s+/g, "");
    }
    // empty invalid
    return " ";
  },

  email: () => {
    const choice = faker.number.int({ min: 1, max: 3 });

    if (choice === 1) {
      return faker.lorem.word() + faker.number + "@invalid.com";
    } else if (choice == 2) {
      return faker.lorem.word() + "@";
    }
    return faker.lorem.word() + "@gmail_com";
  },

  proposalTitle: () => {
    const choice = faker.number.int({ min: 1, max: 2 });
    if (choice === 1) {
      // maximum 80 words invalid
      return faker.lorem.paragraphs(4).replace(/\s+/g, "");
    }
    // empty invalid
    return " ";
  },

  paragraph: () => {
    const choice = faker.number.int({ min: 1, max: 2 });
    if (choice === 1) {
      // maximum 500 words
      return faker.lorem.paragraphs(40);
    }
    // empty invalid
    return " ";
  },

  amount: () => {
    const choice = faker.number.int({ min: 1, max: 2 });
    if (choice === 1) {
      // only number is allowed
      return faker.lorem.word();
    }
    // empty invalid
    return " ";
  },
};
