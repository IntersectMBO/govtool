import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { Footer } from "./Footer";

const mocks = vi.hoisted(() => ({ openFeedbackWindow: vi.fn() }));

vi.mock("@context", () => ({
  useChatwoot: () => ({ openFeedbackWindow: mocks.openFeedbackWindow }),
}));

describe("Footer", () => {
  it("keeps every footer test id and adds the FAQ link", () => {
    render(<Footer />);
    [
      "faqs-link",
      "guides-footer-link",
      "intersect-footer-link",
      "privacy-policy-footer-link",
      "terms-of-use-footer-link",
      "help-footer-button",
      "feedback-footer-button",
    ].forEach((id) => expect(screen.getByTestId(id)).toBeInTheDocument());
  });

  it("leaves the newsletter sign-up to Home", () => {
    render(<Footer />);
    expect(screen.queryByTestId("newsletter-footer-button")).toBeNull();
  });

  it("opens the FAQ in a new tab", () => {
    render(<Footer />);
    expect(screen.getByTestId("faqs-link")).toHaveAttribute(
      "href",
      "https://docs.gov.tools/cardano-govtool/faqs",
    );
    expect(screen.getByTestId("faqs-link")).toHaveAttribute("target", "_blank");
  });

  it("shows the tagline on the redesign's warm background", () => {
    render(<Footer />);
    expect(
      screen.getByText("A clearer connection to Cardano governance."),
    ).toBeInTheDocument();
    expect(screen.getByRole("contentinfo")).toHaveStyle({
      backgroundColor: "#FCFCFA",
    });
  });

  it("opens the feedback window", () => {
    render(<Footer />);
    fireEvent.click(screen.getByTestId("feedback-footer-button"));
    expect(mocks.openFeedbackWindow).toHaveBeenCalled();
  });
});
