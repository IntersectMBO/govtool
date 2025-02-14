import { bech32 } from "bech32";

import I18n from "@/i18n";

/**
 * Validates a value using the Bech32 encoding.
 * @param value - The value to be validated.
 * @returns A boolean indicating whether the value is valid or an error message if it is not valid.
 */
export const bech32Validation = async (value: string) => {
  try {
    const decoded = bech32.decode(value);
    if (decoded.words.length) {
      return true;
    }
    throw new Error();
  } catch (error) {
    return I18n.t("createGovernanceAction.fields.validations.bech32");
  }
};
