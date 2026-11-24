import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import { Typography } from "./Typography";

describe("Typography", () => {
  it("renders paragraphXS at 13/19.5", () => {
    render(<Typography variant="paragraphXS">x</Typography>);
    expect(screen.getByText("x")).toHaveStyle({
      fontSize: "13px",
      lineHeight: "19.5px",
      letterSpacing: "-0.04px",
    });
  });

  it("keeps body2 as 14px medium", () => {
    render(<Typography variant="body2">y</Typography>);
    expect(screen.getByText("y")).toHaveStyle({
      fontSize: "14px",
      lineHeight: "20px",
      fontWeight: "500",
      letterSpacing: "-0.09px",
    });
  });

  it("lets fontWeight override the variant", () => {
    render(
      <Typography variant="paragraphSM" fontWeight={600}>
        z
      </Typography>,
    );
    expect(screen.getByText("z")).toHaveStyle({ fontWeight: "600" });
  });

  it("maps headline4 to the 28px page-title size", () => {
    render(<Typography variant="headline4">t</Typography>);
    expect(screen.getByText("t")).toHaveStyle({
      fontSize: "28px",
      lineHeight: "42px",
      letterSpacing: "-0.52px",
    });
  });
});
