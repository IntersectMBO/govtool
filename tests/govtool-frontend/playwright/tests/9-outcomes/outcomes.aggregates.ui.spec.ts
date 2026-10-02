import { test, expect } from "@playwright/test";
import { readFileSync } from "node:fs";
import type { OutcomeVoteAggregate } from "../../../../../govtool/frontend/src/models/outcomes";

const drep: OutcomeVoteAggregate = {
  role: "drep",
  representation: "stake",
  yes: "67000000",
  no: "3000000",
  abstain: "10000000",
  notVoted: "30000000",
  totalEligible: "110000000",
  threshold: { numerator: 67, denominator: 100 },
};
const spo: OutcomeVoteAggregate = { ...drep, role: "spo" };
const cc: OutcomeVoteAggregate = {
  role: "cc",
  representation: "count",
  yes: "2",
  no: "0",
  abstain: "1",
  notVoted: "1",
  totalEligible: "4",
  threshold: { numerator: 2, denominator: 3 },
};
type Sample = {
  txHash: string;
  index: number;
  type: string;
  status: string;
  epoch: number;
  koios: OutcomeVoteAggregate[];
};
const samples: Sample[] = process.env.OUTCOMES_LIVE_REPORT
  ? JSON.parse(readFileSync(process.env.OUTCOMES_LIVE_REPORT, "utf8")).actions
  : [
      {
        txHash: "aa".repeat(32),
        index: 0,
        type: "HardForkInitiation",
        status: "expired",
        epoch: 500,
        koios: [drep, spo],
      },
      {
        txHash: "bb".repeat(32),
        index: 0,
        type: "HardForkInitiation",
        status: "live",
        epoch: 500,
        koios: [drep, spo, cc],
      },
    ];

for (const sample of samples) {
  test(`${sample.status} ${sample.type} ${sample.txHash.slice(0, 8)} displays independent aggregates`, async ({
    page,
  }) => {
    let networkMetricsRequests = 0;
    const errors: string[] = [];
    page.on("pageerror", (error) => errors.push(error.message));
    const type =
      sample.type === "UpdateCommittee" ? "NewCommittee" : sample.type;
    const ended = sample.status !== "live";
    const action = {
      id: sample.txHash,
      tx_hash: sample.txHash,
      index: sample.index,
      type,
      description:
        type === "NewCommittee"
          ? { members: [], membersToBeRemoved: [], threshold: 0.67 }
          : { major: 11, minor: 0 },
      title: "Aggregate validation",
      abstract: "Action-specific voting results",
      motivation: "",
      rationale: "",
      json_metadata: null,
      url: null,
      data_hash: null,
      proposal_params: null,
      expiry_date: "2026-10-02T00:00:00Z",
      expiration: sample.epoch,
      time: "2026-10-01T00:00:00Z",
      epoch_no: sample.epoch - 2,
      status: {
        ratified_epoch: sample.status === "enacted" ? sample.epoch : null,
        enacted_epoch: sample.status === "enacted" ? sample.epoch + 1 : null,
        expired_epoch: sample.status === "expired" ? sample.epoch : null,
        dropped_epoch: null,
      },
      status_times: {
        ratified_time: null,
        enacted_time: null,
        expired_time: null,
        dropped_time: null,
      },
      prev_gov_action_index: null,
      prev_gov_action_tx_hash: null,
      yes_votes: 0,
      no_votes: 0,
      abstain_votes: 0,
      pool_yes_votes: 0,
      pool_no_votes: 0,
      pool_abstain_votes: 0,
      cc_yes_votes: 0,
      cc_no_votes: 0,
      cc_abstain_votes: 0,
      vote_aggregates: sample.koios,
    };
    await page.route("**/fixture-api/**", async (route) => {
      const url = new URL(route.request().url());
      let json: unknown = null;
      if (url.pathname.includes("/network/metrics")) networkMetricsRequests++;
      else if (
        url.pathname ===
        `/fixture-api/outcomes/governance-actions/${sample.txHash}`
      )
        json = action;
      else if (url.pathname.includes("/epoch/params"))
        json = {
          epoch_no: sample.epoch,
          protocol_major: 11,
          protocol_minor: 0,
        };
      else if (url.pathname.includes("/system/features"))
        json = {
          provider: "govtool-backend",
          network: "preview",
          generatedAt: "2026-10-02T00:00:00Z",
          unavailable: {},
          options: {},
          caveats: [],
        };
      else if (url.pathname.includes("/proposal/")) json = { data: null };
      await route.fulfill({ status: 200, json });
    });
    await page.goto(
      `/outcomes/governance_actions/${sample.txHash}#${sample.index}`
    );
    const panel = page.getByTestId("single-action-outcome-numbers");
    await expect(panel).toBeVisible();
    await expect(panel).toContainText(
      ended
        ? sample.status === "enacted"
          ? "Enacted"
          : "Expired"
        : "In Progress"
    );
    for (const [role, prefix] of [
      ["drep", "DReps"],
      ["spo", "SPOs"],
      ["cc", "CC"],
    ]) {
      const section = page.getByTestId(`${prefix}-voting-results-data`);
      const aggregate = sample.koios.find((a) => a.role === role);
      if (aggregate) {
        await expect(section.getByRole("progressbar")).toBeVisible();
        const title = role === "cc" ? "Constitutional Committee" : prefix;
        await section.getByTestId(`${title}-expand-button`).click();
        const value =
          aggregate.representation === "stake"
            ? `₳ ${((BigInt(aggregate.yes) + 999999n) / 1000000n).toLocaleString("en-US")}`
            : BigInt(aggregate.yes).toLocaleString("en-US");
        await expect(section.getByTestId(`${title}-yes-votes`)).toHaveText(
          value
        );
      } else {
        await expect(section.getByRole("status")).toBeVisible();
        await expect(section.getByRole("progressbar")).toHaveCount(0);
        await expect(
          page.getByTestId(`${prefix}-voting-results-outcome`)
        ).toContainText("-");
      }
    }
    expect(networkMetricsRequests).toBe(0);
    expect(errors).toEqual([]);
  });
}
