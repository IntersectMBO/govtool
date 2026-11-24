import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { Box } from "@mui/material";
import { ThemeProvider } from "@emotion/react";

// GovTool's theme and consts barrel import each other; loading the barrel
// first gives the same evaluation order as the app.
import { ds, FONT_FAMILY, SHADOWS } from "@consts";
import { theme } from "@/theme";

describe("theme", () => {
  it("uses the Figma brand blue", () => {
    expect(theme.palette.primary.main).toBe("#1447E6");
    expect(theme.palette.primaryBlue).toBe("#1447E6");
  });

  it("exposes the design scales on the palette", () => {
    expect(theme.palette.ds.gray["900"]).toBe("#101828");
    expect(theme.palette.ds.primary["600"]).toBe(ds.primary["600"]);
  });

  it("maps legacy text keys to the new grays", () => {
    expect(theme.palette.textBlack).toBe("#101828");
    expect(theme.palette.textGray).toBe("#475467");
  });

  it("uses Figma shadows", () => {
    expect(theme.shadows[1]).toBe(SHADOWS.header);
    expect(theme.shadows[3]).toBe(SHADOWS.cardBlue);
  });

  it("keeps numeric sx radii at MUI's 4px unit, so older screens do not change", () => {
    render(
      <ThemeProvider theme={theme}>
        <Box sx={{ borderRadius: 3 }}>card</Box>
      </ThemeProvider>,
    );
    expect(screen.getByText("card")).toHaveStyle({ borderRadius: "12px" });
  });

  it("sets Geist as the font", () => {
    expect(theme.typography.fontFamily).toBe(FONT_FAMILY.sans);
  });
});
