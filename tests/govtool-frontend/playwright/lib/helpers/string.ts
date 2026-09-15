import { faker } from "@faker-js/faker";

export function extractProposalIdFromUrl(url: string) {
  return parseInt(url.split("/").pop());
}

export function generateExactLengthText(characterLength: number) {
  let text = "";

  // Keep generating paragraphs until we exceed the required length
  while (text.length < characterLength) {
    text += faker.lorem.paragraphs(10);
  }

  // Truncate to the exact number of characters needed
  return text.substring(0, characterLength);
}

export function toCamelCase(str: string) {
  return str
    .toLowerCase()
    .split(" ")
    .map((word, index) =>
      index === 0 ? word : word.charAt(0).toUpperCase() + word.slice(1)
    )
    .join("");
}

export function generateParagraph(isValid: boolean, isEmpty: boolean): string {
  if (isValid) return faker.lorem.paragraph(2);
  const text = isEmpty ? "" : faker.lorem.paragraph(15001);
  return text;
}
