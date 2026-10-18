import { describe, it, expect, vi } from "vitest";
import { createRef } from "react";
import { fireEvent, render, screen } from "@testing-library/react";

// The consts barrel has to start evaluating before the theme (see
// OrderActionsChip.test.tsx).
import "@consts";

import { TextArea } from "./TextArea";

vi.mock("@hooks", () => ({
  useScreenDimension: () => ({ isMobile: false }),
}));

describe("Field.TextArea", () => {
  it("derives the error testid from the message by default", () => {
    render(<TextArea value="" onChange={() => {}} errorMessage="Too long" />);
    expect(screen.getByTestId("too-long-error")).toHaveTextContent("Too long");
  });

  it("uses errorDataTestId for the error message when given", () => {
    render(
      <TextArea
        value=""
        onChange={() => {}}
        errorMessage="Too long"
        errorDataTestId="abstract-helper-error"
      />,
    );
    expect(screen.getByTestId("abstract-helper-error")).toHaveTextContent(
      "Too long",
    );
    expect(screen.queryByTestId("too-long-error")).toBeNull();
  });

  it("renders no error element without a message", () => {
    render(
      <TextArea
        value=""
        onChange={() => {}}
        errorDataTestId="abstract-helper-error"
      />,
    );
    expect(screen.queryByTestId("abstract-helper-error")).toBeNull();
  });

  it("forwards onBlur to the textarea", () => {
    const onBlur = vi.fn();
    render(
      <TextArea
        value=""
        onChange={() => {}}
        onBlur={onBlur}
        data-testid="abstract-input"
      />,
    );
    fireEvent.blur(screen.getByTestId("abstract-input"));
    expect(onBlur).toHaveBeenCalledTimes(1);
  });

  it("calls onBlur once from the imperative blur", () => {
    const onBlur = vi.fn();
    const ref = createRef<HTMLTextAreaElement>();
    render(
      <TextArea
        ref={ref}
        value=""
        onChange={() => {}}
        onBlur={onBlur}
        data-testid="abstract-input"
      />,
    );
    // Not focused: the handle calls onBlur itself, as before.
    ref.current?.blur();
    expect(onBlur).toHaveBeenCalledTimes(1);
    // Focused: the DOM blur event calls it, and only once.
    screen.getByTestId("abstract-input").focus();
    ref.current?.blur();
    expect(onBlur).toHaveBeenCalledTimes(2);
  });
});
