import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";
import type { VoterInfo } from "@/models";

import { RetiredDRepDashboard } from "./RetiredDRepDashboard";
import { ShareDRepDialog } from "./ShareDRepDialog";

const mocks = vi.hoisted(() => ({
  history: {} as Record<string, unknown>,
  votes: [] as unknown[],
  dRep: {} as Record<string, unknown>,
}));

vi.mock("@/context/appContext", () => ({
  useAppContext: () => ({ epochParams: { epoch_no: 600, drep_deposit: 500000000 } }),
}));
vi.mock("@/context/snackbar", () => ({ useSnackbar: () => ({ addSuccessAlert: vi.fn() }) }));
vi.mock("@/hooks/queries/useGetDrepDetailsQuery", () => ({
  useGetDRepDetailsQuery: () => ({ dRep: mocks.dRep, isLoading: false }),
}));
vi.mock("@/hooks/queries/useDRepDashboard", () => ({
  useDRepDashboardVotes: () => ({ votes: mocks.votes, isLoading: false, isError: false }),
  useDRepHistory: () => mocks.history,
  useDRepVotingPowerHistory: () => ({ isServed: false }),
}));

const voter = {
  givenName: "Jordan",
  isRegisteredAsDRep: false,
  wasRegisteredAsDRep: true,
  votingPower: null,
  deposit: 0,
  objectives: "Transparency",
  motivations: null,
} as unknown as VoterInfo;

const vote = (n: number, choice: string) => ({
  vote: { vote: choice, epochNo: 500 + n, date: `2026-09-${10 + n}T00:00:00Z` },
  proposal: { txHash: `t${n}`, index: 0, type: "InfoAction", title: `Action ${n}` },
});

const renderRetired = () =>
  render(
    <MemoryRouter>
      <RetiredDRepDashboard actionCards={<div>cards</div>} dRepID={"cd".repeat(28)} voter={voter} />
    </MemoryRouter>,
  );

describe("RetiredDRepDashboard", () => {
  beforeEach(() => {
    mocks.dRep = {
      view: "drep1retired",
      status: "Retired",
      latestRegistrationDate: "2025-03-15T00:00:00Z",
      votingPower: 3_200_000_000_000,
      motivations: "From the DRep record",
      identityReferences: [],
      linkReferences: [{ "@type": "Links", label: "Blog", uri: "https://blog.example" }],
    };
    mocks.history = { isServed: false };
    mocks.votes = [vote(1, "yes"), vote(2, "no"), vote(3, "abstain"), vote(4, "yes")];
  });

  it("shows the retired state and the way back", () => {
    renderRetired();
    expect(screen.getByTestId("dashboard-role-pill")).toHaveTextContent("Retired");
    expect(screen.getByTestId("retired-drep-banner")).toBeInTheDocument();
    expect(screen.getByTestId("register-button")).toHaveAttribute("href", "/my/drep/register");
    expect(screen.getByTestId("retired-total-votes")).toHaveTextContent("4 proposals");
    expect(screen.getAllByTestId("retired-archived-vote")).toHaveLength(3);
    expect(screen.getByText("cards")).toBeInTheDocument();
  });

  it("asks for today's deposit to return, and dates no refund it cannot see", () => {
    renderRetired();
    expect(screen.getByTestId("retired-return-card")).toHaveTextContent("500 ADA deposit");
    expect(screen.getByTestId("retired-deposit-note")).toHaveTextContent(
      "Your DRep deposit was returned to your wallet when the retirement was confirmed.",
    );
  });

  it("fills profile fields the voter record leaves empty from the DRep record", () => {
    renderRetired();
    expect(screen.getByText("From the DRep record")).toBeInTheDocument();
  });

  it("shows a dash for an unknown final voting power", () => {
    mocks.dRep = { ...mocks.dRep, votingPower: undefined };
    renderRetired();
    expect(screen.getByTestId("retired-final-power")).toHaveTextContent("–");
  });

  it("shows the registration date until the retirement date is known", () => {
    renderRetired();
    expect(screen.getByTestId("retired-tenure")).toHaveTextContent("March 15, 2025");
  });

  it("shows the tenure once the history dates the retirement", () => {
    mocks.history = {
      isServed: true,
      data: {
        elements: [{ kind: "retired", status: "retired", at: { epoch: 590, time: "2026-08-20T00:00:00Z" }, txHash: "r", anchor: null }],
      },
    };
    renderRetired();
    expect(screen.getByTestId("retired-tenure")).toHaveTextContent("17 months");
  });
});

describe("ShareDRepDialog", () => {
  it("shows a shortened DRep id but copies the whole URL, under the test ids it is given", () => {
    const writeText = vi.fn(() => Promise.resolve());
    Object.assign(navigator, { clipboard: { writeText } });
    const id = "drep1example7m4k2v9n6p3qwertyuiopasdfghjklp2q9r6s3t8v5";
    render(
      <ShareDRepDialog
        buttonTestId="share-button"
        copyTestId="copy-link-from-share-button"
        dRepId={id}
        link={`https://gov.tools/dreps/${id}`}
      />,
    );
    fireEvent.click(screen.getByTestId("share-button"));
    expect(screen.getByTestId("share-drep-url")).toHaveValue("drep1example7m4k2v…p2q9r6s3t8v5");
    fireEvent.click(screen.getByTestId("copy-link-from-share-button"));
    expect(writeText).toHaveBeenCalledWith(`https://gov.tools/dreps/${id}`);
  });

  const link = "https://gov.tools/dreps/drep1abc";

  it("copies the profile URL and offers encoded share links", async () => {
    const writeText = vi.fn(() => Promise.resolve());
    Object.assign(navigator, { clipboard: { writeText } });
    render(<ShareDRepDialog link={link} />);
    fireEvent.click(screen.getByTestId("share-drep-button"));
    expect(screen.getByTestId("share-drep-url")).toHaveValue(link);
    fireEvent.click(screen.getByTestId("share-drep-copy"));
    expect(writeText).toHaveBeenCalledWith(link);
    const x = screen.getByTestId("share-drep-x");
    expect(x.getAttribute("href")).toContain(encodeURIComponent(link));
    expect(x).toHaveAttribute("rel", "noopener noreferrer");
    expect(x).toHaveAttribute("target", "_blank");
    expect(screen.getByTestId("share-drep-email").getAttribute("href")).toMatch(/^mailto:\?/);
  });
});
