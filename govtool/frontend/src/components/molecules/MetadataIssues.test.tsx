import { render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import "@/i18n";
import { MetadataIssue, MetadataValidationStatus } from "@models";

import { DataMissingInfoBox } from "./DataMissingInfoBox";
import { MetadataWarningInfoBox } from "./MetadataWarningInfoBox";

// The atoms barrel pulls in every context provider; Typography is enough.
vi.mock("@atoms", async () => ({
  Typography: (await import("../atoms/Typography")).Typography,
}));

vi.mock("@hooks", async () => {
  const { useTranslation } = await import("react-i18next");
  return { useTranslation };
});

const titleTooLong: MetadataIssue = {
  field: "title",
  rule: "maxLength",
  severity: "warning",
  limit: 80,
  actual: 84,
};

describe("DataMissingInfoBox", () => {
  it.each([
    [
      MetadataValidationStatus.URL_BLOCKED,
      "The URL this Governance Action was posted with points to an address GovTool does not fetch from.",
    ],
    [
      MetadataValidationStatus.EXCEEDS_LIMIT,
      "The data that was originally used when this Governance Action was created is too large to load.",
    ],
    [
      MetadataValidationStatus.INTERNAL_ERROR,
      "GovTool could not check the data for this Governance Action.",
    ],
  ])("has a message for %s", (status, message) => {
    render(<DataMissingInfoBox isDataMissing={status} />);
    expect(screen.getByTestId("metadata-error-message").textContent).toBe(
      message,
    );
    expect(
      screen.getByTestId("metadata-error-description").textContent,
    ).not.toBe("");
  });

  it("lists the missing fields under a format failure", () => {
    render(
      <DataMissingInfoBox
        isDataMissing={MetadataValidationStatus.INCORRECT_FORMAT}
        issues={[
          { field: "rationale", rule: "required", severity: "error" },
          titleTooLong,
        ]}
      />,
    );
    const list = screen.getByTestId("metadata-error-issues");
    expect(list.textContent).toBe("Rationale is missing or empty.");
  });
});

describe("MetadataWarningInfoBox", () => {
  it("names the over-long field and its lengths", () => {
    render(<MetadataWarningInfoBox issues={[titleTooLong]} />);
    expect(screen.getByTestId("metadata-warning").textContent).toContain(
      "Title is 84 characters long; the standard allows 80.",
    );
  });

  it("renders nothing without warnings", () => {
    const { container } = render(
      <MetadataWarningInfoBox
        issues={[{ field: "title", rule: "required", severity: "error" }]}
      />,
    );
    expect(container.innerHTML).toBe("");
  });
});
