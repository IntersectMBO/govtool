import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import { PageTitle } from "./PageTitle";

describe("PageTitle", () => {
  it("renders the title at the 28px page-title size", () => {
    render(<PageTitle title="Governance actions" />);
    expect(screen.getByText("Governance actions")).toHaveStyle({
      fontSize: "28px",
      color: "#101828",
    });
  });

  it("renders a gray description under the title", () => {
    render(
      <PageTitle title="Governance actions" description="Explore proposals" />,
    );
    expect(screen.getByText("Explore proposals")).toHaveStyle({
      color: "#667085",
      fontSize: "14px",
    });
  });

  it("renders an action beside the title", () => {
    render(
      <PageTitle
        title="Governance actions"
        action={<button type="button">Create</button>}
      />,
    );
    expect(screen.getByRole("button", { name: "Create" })).toBeInTheDocument();
  });
});
