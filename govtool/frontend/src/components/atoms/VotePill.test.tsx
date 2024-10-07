import { describe, it, expect } from "vitest";
import { render } from "@testing-library/react";
import { VotePill } from "@atoms";

describe("VotePill", () => {
  it('renders the VotePill component with "yes" vote correctly', () => {
    const { getByText } = render(<VotePill vote="yes" />);
    const voteText = getByText("Yes");
    expect(voteText).toBeInTheDocument();
    expect(voteText.parentNode).toHaveStyle({
      borderColor: "#C0E4BA",
      backgroundColor: "#F0F9EE",
    });
  });

  it('renders the VotePill component with "no" vote correctly', () => {
    const { getByText } = render(<VotePill vote="no" />);
    const voteText = getByText("No");
    expect(voteText).toBeInTheDocument();
    expect(voteText.parentNode).toHaveStyle({
      borderColor: "#EDACAC",
      backgroundColor: "#FBEBEB",
    });
  });

  it('renders the VotePill component with "abstain" vote correctly', () => {
    const { getByText } = render(<VotePill vote="abstain" />);
    const voteText = getByText("Abstain");
    expect(voteText).toBeInTheDocument();
    expect(voteText.parentNode).toHaveStyle({
      borderColor: "#99ADDE",
      backgroundColor: "#E6EBF7",
    });
  });

  it("handles custom width and maxWidth props correctly", () => {
    const { container } = render(
      <VotePill vote="yes" width={100} maxWidth={120} />,
    );
    const pillBox = container.firstChild;
    expect(pillBox).toHaveStyle({
      width: "100px",
      maxWidth: "120px",
    });
  });

  it("defaults width and maxWidth when not provided", () => {
    const { container } = render(<VotePill vote="abstain" />);
    const pillBox = container.firstChild;
    expect(pillBox).toHaveStyle({
      width: "auto",
      maxWidth: "auto",
    });
  });
});
