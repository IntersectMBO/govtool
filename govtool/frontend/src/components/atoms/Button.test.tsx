import { ReactNode } from "react";
import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { ThemeProvider } from "@emotion/react";

// GovTool's theme and consts barrel import each other; loading the barrel
// first gives the same evaluation order as the app.
import "@consts";
import { theme } from "@/theme";

import { Button } from "./Button";

const renderInTheme = (node: ReactNode) =>
  render(<ThemeProvider theme={theme}>{node}</ThemeProvider>);

describe("Button", () => {
  it("is a 40px brand-blue pill by default", () => {
    renderInTheme(<Button>Go</Button>);
    expect(screen.getByRole("button", { name: "Go" })).toHaveStyle({
      backgroundColor: "#1447E6",
      height: "40px",
      borderRadius: "999px",
    });
  });

  it("has a soft variant for secondary actions", () => {
    renderInTheme(<Button variant="soft">Soft</Button>);
    expect(screen.getByRole("button", { name: "Soft" })).toHaveStyle({
      backgroundColor: "#E0E9FF",
      color: "#0038C7",
    });
  });

  it("is orange with the secondary colour, dark enough for white text", () => {
    renderInTheme(<Button color="secondary">Become a DRep</Button>);
    expect(screen.getByRole("button", { name: "Become a DRep" })).toHaveStyle({
      backgroundColor: "#C94F22",
    });
  });

  it("is 48px with 16px text at extraLarge", () => {
    renderInTheme(<Button size="extraLarge">Big</Button>);
    expect(screen.getByRole("button", { name: "Big" })).toHaveStyle({
      height: "48px",
      fontSize: "16px",
    });
  });

  it("is outlined in gray", () => {
    renderInTheme(<Button variant="outlined">Back</Button>);
    expect(screen.getByRole("button", { name: "Back" })).toHaveStyle({
      backgroundColor: "#FFFFFF",
      color: "#344054",
    });
  });

  it("disables itself and shows a spinner while loading", () => {
    renderInTheme(<Button isLoading>Wait</Button>);
    expect(screen.getByRole("button", { name: "Wait" })).toBeDisabled();
    expect(screen.getByRole("progressbar")).toBeInTheDocument();
  });

  it("renders its dataTestId", () => {
    renderInTheme(<Button dataTestId="go-button">Go</Button>);
    expect(screen.getByTestId("go-button")).toBeInTheDocument();
  });
});
