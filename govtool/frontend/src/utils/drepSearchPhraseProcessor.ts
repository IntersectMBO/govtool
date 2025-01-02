import { decodeCIP129Identifier } from "./cip129identifier";

/**
 * Processes the search phrase for dRep and returns the dRep ID.
 * If the phrase starts with "drep_script" or "drep",
 * it decodes the CIP129 identifier and extracts the transaction ID.
 * If the DRep ID starts with "22" or "23", it returns the ID without the prefix.
 * If any error occurs during processing, it returns the original phrase.
 *
 * @param phrase - The search phrase to be processed.
 * @returns The dRep ID extracted from the search phrase or the original phrase if an error occurs.
 */
export const dRepSearchPhraseProcessor = async (phrase: string) => {
  let drepIDPhrase = phrase;

  try {
    if (
      drepIDPhrase.startsWith("drep_script") ||
      drepIDPhrase.startsWith("drep")
    ) {
      const { txID } = decodeCIP129Identifier(drepIDPhrase);

      drepIDPhrase = txID;
    }
    if (drepIDPhrase.startsWith("22") || drepIDPhrase.startsWith("23")) {
      return drepIDPhrase.slice(2);
    }

    return drepIDPhrase;
  } catch (e) {
    return phrase;
  }
};
