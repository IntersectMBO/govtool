import { describe, it, expect, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

// The consts barrel has to start evaluating before the theme (see
// OrderActionsChip.test.tsx).
import "@consts";

import { Input } from "./Input";

describe("Field.Input", () => {
  it("puts dataTestId on the input and errorDataTestId on the error", () => {
    render(
      <Input
        value=""
        onChange={() => {}}
        dataTestId="title-input"
        errorMessage="Required"
        errorDataTestId="title-input-error"
      />,
    );
    expect(screen.getByTestId("title-input").tagName).toBe("INPUT");
    expect(screen.getByTestId("title-input-error")).toHaveTextContent(
      "Required",
    );
  });

  it("forwards onBlur to the input", () => {
    const onBlur = vi.fn();
    render(
      <Input
        value=""
        onChange={() => {}}
        onBlur={onBlur}
        dataTestId="title-input"
      />,
    );
    fireEvent.blur(screen.getByTestId("title-input"));
    expect(onBlur).toHaveBeenCalledTimes(1);
  });
});
