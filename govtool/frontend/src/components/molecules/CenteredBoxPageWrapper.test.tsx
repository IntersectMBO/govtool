import { describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

vi.mock("@organisms", () => ({
  TopNav: () => null,
  Footer: () => null,
}));
vi.mock("@atoms", () => ({
  Background: ({ children }: { children: React.ReactNode }) => <div>{children}</div>,
}));
vi.mock("@consts", () => ({ PATHS: { dashboard: "/dashboard" } }));
vi.mock("@hooks", async () => ({
  useTranslation: (await import("react-i18next")).useTranslation,
}));

// eslint-disable-next-line import/first
import { CenteredBoxPageWrapper } from "./CenteredBoxPageWrapper";

describe("CenteredBoxPageWrapper", () => {
  it("names the flow when it has no stepper", () => {
    render(
      <MemoryRouter>
        <CenteredBoxPageWrapper pageTitle="Retire as a Direct Voter">
          <p>step</p>
        </CenteredBoxPageWrapper>
      </MemoryRouter>,
    );
    expect(screen.getByText("Retire as a Direct Voter")).toBeVisible();
  });

  it("names the flow in the stepper", () => {
    render(
      <MemoryRouter>
        <CenteredBoxPageWrapper
          pageTitle="Become a DRep"
          stepper={{ steps: ["Understand", "Profile"], current: 1 }}
        >
          <p>step</p>
        </CenteredBoxPageWrapper>
      </MemoryRouter>,
    );
    expect(screen.getByTestId("flow-stepper")).toHaveTextContent("Become a DRep");
    expect(screen.getAllByText("Become a DRep")).toHaveLength(1);
  });
});
