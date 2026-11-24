import { ReactNode } from "react";
import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { ThemeProvider } from "@emotion/react";

// GovTool's theme and consts barrel import each other; loading the barrel
// first gives the same evaluation order as the app.
import "@consts";
import { theme } from "@/theme";

import { Input } from "./Input";
import { TextArea } from "./TextArea";

const renderInTheme = (node: ReactNode) =>
  render(<ThemeProvider theme={theme}>{node}</ThemeProvider>);

describe("Input", () => {
  it("is an 8px-radius field with a light gray border", () => {
    renderInTheme(<Input dataTestId="field" />);
    const root = screen.getByTestId("field").parentElement as HTMLElement;
    expect(root).toHaveStyle({
      borderRadius: "8px",
      borderColor: "#E5E9F0",
      height: "38px",
    });
  });

  it("turns light red with an error", () => {
    renderInTheme(<Input dataTestId="field" errorMessage="Required" />);
    const root = screen.getByTestId("field").parentElement as HTMLElement;
    expect(root).toHaveStyle({
      backgroundColor: "#FEF3F2",
      borderColor: "#F04438",
    });
  });
});

describe("TextArea", () => {
  it("uses the same border and an 8px radius", () => {
    renderInTheme(<TextArea data-testid="area" />);
    expect(screen.getByTestId("area")).toHaveStyle({
      borderRadius: "8px",
      border: "1px solid #E5E9F0",
    });
  });
});
