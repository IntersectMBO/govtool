import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import { Background } from "./Background";
import { PageContainer } from "./PageContainer";

describe("PageContainer", () => {
  it("centres content at the redesign's 1180px width", () => {
    render(<PageContainer>content</PageContainer>);
    expect(screen.getByText("content")).toHaveStyle({
      maxWidth: "1180px",
      width: "100%",
    });
  });
});

describe("Background", () => {
  it("drops the old orange and blue blobs for a top glow", () => {
    render(<Background>page</Background>);
    expect(screen.queryByAltText("bg-orange")).toBeNull();
    expect(screen.queryByAltText("bg-blue")).toBeNull();
    expect(screen.getByTestId("page-glow")).toBeInTheDocument();
    expect(screen.getByText("page")).toBeInTheDocument();
  });
});
