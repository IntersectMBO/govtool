import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import { Breadcrumbs } from "./Breadcrumbs";

describe("Breadcrumbs", () => {
  it("shows the parent in gray and the current page in dark semibold", () => {
    render(
      <MemoryRouter>
        <Breadcrumbs
          elementOne="Governance Actions"
          elementOnePath="/actions"
          elementTwo="Reduce minPoolCost"
          isDataMissing={null}
        />
      </MemoryRouter>,
    );
    expect(screen.getByText("Governance Actions")).toHaveStyle({
      color: "#667085",
    });
    expect(screen.getByText("Reduce minPoolCost")).toHaveStyle({
      color: "#101828",
      fontWeight: "600",
    });
    expect(screen.getByTestId("breadcrumb-component")).toBeInTheDocument();
  });
});
