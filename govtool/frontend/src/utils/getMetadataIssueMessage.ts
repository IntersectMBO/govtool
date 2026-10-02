import i18n from "@/i18n";
import { MetadataIssue } from "@/models";

/** One sentence naming the field and the rule it breaks. */
export const getMetadataIssueMessage = (issue: MetadataIssue): string => {
  const field = i18n.t(`metadataIssues.fields.${issue.field}`, {
    defaultValue: issue.field,
  });

  return issue.rule === "maxLength"
    ? i18n.t("metadataIssues.maxLength", {
        field,
        actual: issue.actual,
        limit: issue.limit,
      })
    : i18n.t("metadataIssues.required", { field });
};

export const getMetadataWarnings = (issues?: MetadataIssue[]) =>
  issues?.filter((issue) => issue.severity === "warning") ?? [];

export const getMetadataErrors = (issues?: MetadataIssue[]) =>
  issues?.filter((issue) => issue.severity === "error") ?? [];
