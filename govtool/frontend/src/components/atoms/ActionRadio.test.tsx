import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { ActionRadio } from "@atoms";

describe("ActionRadio", () => {
  it("should execute onChange with the correct value on click", () => {
    const handleChange = vi.fn();
    render(
      <ActionRadio
        title="Test Radio"
        value="test-value"
        onChange={handleChange}
        dataTestId="action-radio"
      />,
    );

    const radio = screen.getByTestId("action-radio");
    fireEvent.click(radio);

    expect(handleChange).toHaveBeenCalledTimes(1);
    expect(handleChange).toHaveBeenCalledWith("test-value");
  });

  it("should change styles based on isChecked change", () => {
    const { rerender } = render(
      <ActionRadio
        title="Test Radio"
        value="unchecked-value"
        isChecked={false}
        onChange={() => {}}
        dataTestId="action-radio"
      />,
    );
    let radio = screen.getByTestId("action-radio");

    expect(radio).toHaveStyle("borderColor: white");
    expect(radio).toHaveStyle("backgroundColor: rgb(255, 255, 255)");

    rerender(
      <ActionRadio
        title="Test Radio"
        value="checked-value"
        isChecked
        onChange={() => {}}
        dataTestId="action-radio"
      />,
    );
    radio = screen.getByTestId("action-radio");

    expect(radio).toHaveStyle("borderColor: specialCyanBorder");
    expect(radio).toHaveStyle("backgroundColor: specialCyan");
  });

  it("should display correct title and optional subtitle", () => {
    render(
      <ActionRadio
        title="Main Title"
        subtitle="Sub Title"
        value="any-value"
        onChange={() => {}}
        dataTestId="action-radio"
      />,
    );

    const title = screen.getByText("Main Title");
    const subtitle = screen.getByText("Sub Title");

    expect(title).toBeInTheDocument();
    expect(subtitle).toBeInTheDocument();
  });

  it("should display tooltip text when InfoOutlinedIcon is hovered over", async () => {
    render(
      <ActionRadio
        title="With Tooltip"
        tooltipText="Info Here"
        tooltipTitle="Tooltip"
        value="tooltip-present-value"
        onChange={() => {}}
        dataTestId="action-radio"
      />,
    );

    const icon = screen.getByTestId("InfoOutlinedIcon");
    fireEvent.mouseOver(icon);

    const tooltip = await screen.findByText("Info Here", {}, { timeout: 500 });
    expect(tooltip).toBeInTheDocument();
  });
});
