import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { SnackbarProvider } from "@context";
import { CopyButton } from "@atoms";
import { act } from "react";

Object.defineProperty(global.navigator, "clipboard", {
  value: {
    writeText: vi.fn(),
  },
  writable: true,
});

vi.mock("@hooks", () => ({
  useTranslation: () => ({
    t: (key: string) => key,
  }),
  useScreenDimension: () => ({
    isMobile: false,
  }),
}));

const writeTextMock = navigator.clipboard.writeText as unknown as {
  mockClear: () => void;
};

describe("CopyButton", () => {
  beforeEach(() => {
    writeTextMock.mockClear();
  });

  it("renders correctly with the default icon", () => {
    render(<CopyButton text="Example Text" />);
    const image = screen.getByRole("img");
    expect(image).toHaveAttribute("src", "/icons/Copy.svg");
  });

  it("renders the blue icon when variant is 'blue'", () => {
    render(<CopyButton text="Example Text" variant="blue" />);
    const image = screen.getByRole("img");
    expect(image).toHaveAttribute("src", "/icons/CopyBlue.svg");
  });

  it("renders the blue thin icon when variant is 'blueThin'", () => {
    render(<CopyButton text="Example Text" variant="blueThin" />);
    const image = screen.getByRole("img");
    expect(image).toHaveAttribute("src", "/icons/CopyBlueThin.svg");
  });

  it("renders the white icon when isChecked prop is true", () => {
    render(<CopyButton text="Example Text" isChecked />);
    const image = screen.getByRole("img");
    expect(image).toHaveAttribute("src", "/icons/CopyWhite.svg");
  });

  it("copies text to clipboard and shows success alert on click", async () => {
    render(
      <SnackbarProvider>
        <CopyButton text="Example Text" />,
      </SnackbarProvider>,
    );

    const copyButton = screen.getByTestId("copy-button");

    await act(async () => {
      await userEvent.click(copyButton);
    });

    expect(navigator.clipboard.writeText).toHaveBeenCalledWith("Example Text");
  });
});
