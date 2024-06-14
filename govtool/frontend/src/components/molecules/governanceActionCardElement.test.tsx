import { render, screen } from "@testing-library/react";
import { GovernanceActionCardElement } from "./GovernanceActionCardElement"; // Adjust the import path as per your file structure

describe("GovernanceActionCardElement", () => {
  it("renders markdown content correctly", () => {
    const label = "Example Label";
    const markdownText = "**Bold**";

    render(
      <GovernanceActionCardElement
        label={label}
        text={markdownText}
        isMarkdown
      />,
    );

    expect(screen.getByText(label)).toBeInTheDocument();
    expect(screen.getByText("Bold")).toBeInTheDocument();

    const strongElement = screen.getByText("Bold").closest("strong");
    expect(strongElement).toBeInTheDocument();
  });
});
