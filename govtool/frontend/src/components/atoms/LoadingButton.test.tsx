import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { LoadingButton } from "@atoms";

describe("LoadingButton", () => {
  it("renders its children", () => {
    render(<LoadingButton isLoading={false}>Click me</LoadingButton>);
    expect(screen.getByText("Click me")).toBeInTheDocument();
  });

  it("is disabled when isLoading is true", () => {
    render(<LoadingButton isLoading>Loading...</LoadingButton>);
    expect(screen.getByRole("button", { name: "Loading..." })).toBeDisabled();
  });

  it("is disabled when disabled prop is true", () => {
    render(<LoadingButton disabled>Disabled</LoadingButton>);
    expect(screen.getByRole("button", { name: "Disabled" })).toBeDisabled();
  });

  it("shows a CircularProgress when isLoading", () => {
    render(<LoadingButton isLoading>Loading...</LoadingButton>);
    expect(screen.getByRole("progressbar")).toBeInTheDocument();
  });

  it("applies different heights based on size prop", () => {
    const { rerender } = render(
      <LoadingButton size="small">Small Button</LoadingButton>,
    );

    expect(screen.getByText("Small Button")).toHaveStyle({ height: "32px" });

    rerender(<LoadingButton size="medium">Medium Button</LoadingButton>);
    expect(screen.getByText("Medium Button")).toHaveStyle({ height: "36px" });

    rerender(<LoadingButton size="large">Large Button</LoadingButton>);
    expect(screen.getByText("Large Button")).toHaveStyle({ height: "40px" });

    rerender(
      <LoadingButton size="extraLarge">Extra Large Button</LoadingButton>,
    );
    expect(screen.getByText("Extra Large Button")).toHaveStyle("height: 48px");
  });

  it("applies custom styles via sx prop", () => {
    const customStyles = { backgroundColor: "specialCyan" };
    render(<LoadingButton sx={customStyles}>Styled Button</LoadingButton>);

    expect(screen.getByText("Styled Button")).toHaveStyle({
      backgroundColor: "specialCyan",
    });
  });
});
