import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { VoterInfo } from "@models";
import { DashboardGovernanceActions } from "./DashboardGovernanceActions";

let voter: Partial<VoterInfo> | undefined;

vi.mock("@hooks", () => ({
  useGetVoterInfo: () => ({ voter }),
  useScreenDimension: () => ({ isMobile: false }),
  useTranslation: () => ({ t: (key: string) => key }),
}));

vi.mock("@context", () => ({
  useCardano: () => ({ isEnableLoading: false }),
  useDataActionsBar: () => ({ debouncedSearchText: "" }),
  useFeatureFlag: () => ({
    isProposalDiscussionForumEnabled: false,
    governanceActionsSort: { options: [], isAvailable: false },
  }),
}));

vi.mock("@molecules", () => ({ DataActionsBar: () => null }));

vi.mock("@atoms", () => ({ Button: () => null }));

// The lists are out of scope: show which one the page renders.
vi.mock("@organisms", () => ({
  GovernanceActionsToVote: () => <p data-testid="to-vote-list" />,
  DashboardGovernanceActionsVotedOn: () => <p data-testid="voted-on-list" />,
}));

const renderBackFromVotedAction = () =>
  render(
    <MemoryRouter
      initialEntries={[{ pathname: "/", state: { isVotedListOnLoad: true } }]}
    >
      <DashboardGovernanceActions />
    </MemoryRouter>,
  );

describe("DashboardGovernanceActions", () => {
  beforeEach(() => {
    voter = undefined;
  });

  it("opens on the voted-on tab for a DRep coming back from its vote", () => {
    voter = { isRegisteredAsDRep: true };
    renderBackFromVotedAction();

    expect(screen.getByTestId("voted-on-list")).toBeInTheDocument();
    expect(screen.queryByTestId("to-vote-list")).toBeNull();
  });

  it("shows every action to a retired DRep coming back from its old vote", () => {
    voter = { isRegisteredAsDRep: false, wasRegisteredAsDRep: true };
    renderBackFromVotedAction();

    expect(screen.queryByTestId("voted-tab")).toBeNull();
    expect(screen.getByTestId("to-vote-list")).toBeInTheDocument();
    expect(screen.queryByTestId("voted-on-list")).toBeNull();
  });
});
