import { faker } from "@faker-js/faker";

export function extractProposalIdFromUrl(url: string) {
  return parseInt(url.split("/").pop());
}

export function generateExactLengthText(characterLength:number) {
  let text = '';
  
  // Keep generating paragraphs until we exceed the required length
  while (text.length < characterLength) {
      text += faker.lorem.paragraphs(10);
  }
  
  // Truncate to the exact number of characters needed
  return text.substring(0, characterLength);
}