import { faker } from "@faker-js/faker";

export const valid = {
  username: () => {
    let timeStamp = Date.now();
    let username = `${faker.internet.displayName().toLowerCase()}_${timeStamp}`;

    // Remove any invalid characters
    username = username.replace(/[^a-z0-9._]/g, "");

    // Ensure the username is between 1 and 30 characters
    if (username.length > 30) {
      username = username.substring(0, 30);
    }

    // Ensure the first character is not a period or underscore
    if (username.startsWith(".") || username.startsWith("_")) {
      username = "a" + username.substring(1);
    }

    // Ensure the username is not empty after the transformations
    if (username.length === 0) {
      username = "user" + faker.number.int({ min: 1, max: 9999 });
    }

    return username;
  }
};
