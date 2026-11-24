import { describe, expect, it } from "vitest";

import "@consts";
import type { VotedProposal } from "@/models";
import { groupVotesByDay } from "./DRepVotingTrend";

const vote = (date: string, choice: "yes" | "no" | "abstain") =>
  ({ vote: { date, vote: choice }, proposal: {} }) as unknown as VotedProposal;

describe("groupVotesByDay", () => {
  const today = new Date("2026-11-16T12:00:00");

  it("makes one bucket per day, oldest first, ending today", () => {
    const days = groupVotesByDay([], 7, today);
    expect(days).toHaveLength(7);
    expect(days[6].date.getDate()).toBe(16);
    expect(days[0].date.getDate()).toBe(10);
  });

  it("counts each vote on its day", () => {
    const days = groupVotesByDay(
      [
        vote("2026-11-16T08:00:00", "yes"),
        vote("2026-11-16T09:00:00", "no"),
        vote("2026-11-15T10:00:00", "abstain"),
      ],
      7,
      today,
    );
    expect(days[6]).toMatchObject({ yes: 1, no: 1, abstain: 0 });
    expect(days[5]).toMatchObject({ yes: 0, no: 0, abstain: 1 });
  });

  it("ignores votes without a date or outside the range", () => {
    const days = groupVotesByDay(
      [vote("", "yes"), vote("2026-10-01T10:00:00", "yes")],
      7,
      today,
    );
    expect(days.every((day) => day.yes === 0)).toBe(true);
  });
});
