import { render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { DataMissingHeader } from "./DataMissingHeader";

// A phone-sized screen.
vi.mock("@/hooks", () => ({
  useScreenDimension: () => ({ screenWidth: 375, isMobile: true }),
}));

// The atoms barrel pulls in every context provider; the title is all the
// header needs from it.
vi.mock("@atoms", () => ({
  Typography: ({ children }: { children: React.ReactNode }) => (
    <h1>{children}</h1>
  ),
}));

vi.mock("./Share", () => ({
  Share: ({ link }: { link: string }) => (
    <button type="button" data-testid="share-button" data-link={link}>
      share
    </button>
  ),
}));

describe("DataMissingHeader", () => {
  it("offers the share button on a small screen", () => {
    render(
      <DataMissingHeader isDRep title="Alice" shareLink="https://x/drep" />,
    );

    expect(screen.getByTestId("share-button")).toHaveAttribute(
      "data-link",
      "https://x/drep",
    );
  });
});
