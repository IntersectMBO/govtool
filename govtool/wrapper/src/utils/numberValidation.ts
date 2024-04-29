import I18n from "@/i18n";

/**
 * Validates a string value as a number.
 *
 * @param value - The string value to be validated.
 * @returns Either an error message or `true` if the value is a valid number.
 */
export const numberValidation = (value: string) => {
  const parsedValue = Number(
    value.includes(",") ? value.replace(",", ".") : value,
  );

  if (Number.isNaN(parsedValue)) {
    return I18n.t("createGovernanceAction.fields.validations.number");
  }

  if (parsedValue < 0) {
    return I18n.t("createGovernanceAction.fields.validations.positive");
  }

  return true;
};
