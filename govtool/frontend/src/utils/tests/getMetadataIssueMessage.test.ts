import { MetadataIssue } from "@models";
import {
  getMetadataErrors,
  getMetadataIssueMessage,
  getMetadataWarnings,
} from "../getMetadataIssueMessage";

describe("getMetadataIssueMessage", () => {
  it("names the field and both lengths for an over-long field", () => {
    expect(
      getMetadataIssueMessage({
        field: "title",
        rule: "maxLength",
        severity: "warning",
        limit: 80,
        actual: 84,
      }),
    ).toBe("Title is 84 characters long; the standard allows 80.");
  });

  it("names a missing field", () => {
    expect(
      getMetadataIssueMessage({
        field: "givenName",
        rule: "required",
        severity: "error",
      }),
    ).toBe("Name is missing or empty.");
  });

  it("falls back to the raw field name for an unknown field", () => {
    expect(
      getMetadataIssueMessage({
        field: "references",
        rule: "required",
        severity: "error",
      }),
    ).toBe("references is missing or empty.");
  });

  it("splits issues by severity", () => {
    const issues: MetadataIssue[] = [
      { field: "rationale", rule: "required", severity: "error" },
      {
        field: "abstract",
        rule: "maxLength",
        severity: "warning",
        limit: 2500,
        actual: 3000,
      },
    ];
    expect(getMetadataErrors(issues)).toEqual([issues[0]]);
    expect(getMetadataWarnings(issues)).toEqual([issues[1]]);
    expect(getMetadataWarnings(undefined)).toEqual([]);
  });
});
