import { test, expect } from "@playwright/test";
import { readFileSync } from "node:fs";
import type { GovernanceActionVoteAggregate } from "../../../../../govtool/frontend/src/models/governanceActions";

const drep: GovernanceActionVoteAggregate = {
  role: "drep",
  representation: "stake",
  yes: "67000000",
  no: "3000000",
  abstain: "10000000",
  notVoted: "30000000",
  totalEligible: "110000000",
  threshold: { numerator: 67, denominator: 100 },
};
const spo: GovernanceActionVoteAggregate = { ...drep, role: "spo" };
const cc: GovernanceActionVoteAggregate = {
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
  status: "live" | "ratified" | "enacted" | "expired" | "dropped";
  epoch: number;
  koios: GovernanceActionVoteAggregate[];
  scenario?: string;
  inconsistentRole?: GovernanceActionVoteAggregate["role"];
};
const fixture = (txByte: string, overrides: Partial<Sample>): Sample => ({
  txHash: txByte.repeat(32),
  index: 0,
  type: "HardForkInitiation",
  status: "live",
  epoch: 500,
  koios: [drep, spo, cc],
  ...overrides,
});
const samples: Sample[] = process.env.GOVTOOL_AGGREGATES_REPORT
  ? JSON.parse(readFileSync(process.env.GOVTOOL_AGGREGATES_REPORT, "utf8"))
      .actions
  : [
      fixture("aa", { status: "expired", koios: [drep, spo] }),
      fixture("bb", {}),
      fixture("cc", { status: "ratified" }),
      fixture("dd", { status: "enacted" }),
      fixture("ee", { status: "dropped" }),
      fixture("ab", { type: "InfoAction" }),
      fixture("ac", {
        scenario: "all-abstaining",
        koios: [
          {
            ...drep,
            yes: "0",
            no: "0",
            abstain: drep.totalEligible,
            notVoted: "0",
          },
        ],
      }),
      fixture("ad", {
        scenario: "fractional percentages",
        koios: [
          {
            ...drep,
            representation: "percent",
            yes: "0.4",
            no: "0.1",
            abstain: "0.2",
            notVoted: "0.3",
            totalEligible: "1",
          },
        ],
      }),
      fixture("ae", {
        scenario: "inconsistent DRep data",
        inconsistentRole: "drep",
        koios: [{ ...drep, yes: "0" }, spo],
      }),
      fixture("af", { type: "ParameterChange" }),
      fixture("ba", { type: "TreasuryWithdrawals", koios: [drep, cc] }),
      fixture("bc", {
        scenario: "provider veto",
        koios: [drep, spo, { ...cc, passing: false }],
      }),
    ];

for (const sample of samples) {
  test(`${sample.status} ${sample.type} ${sample.scenario ?? sample.txHash.slice(0, 8)} displays independent aggregates`, async ({
    page,
  }) => {
    let networkMetricsRequests = 0;
    const errors: string[] = [];
    page.on("pageerror", (error) => errors.push(error.message));
    const type =
      sample.type === "UpdateCommittee" ? "NewCommittee" : sample.type;
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
        ratified_epoch: ["ratified", "enacted"].includes(sample.status)
          ? sample.epoch
          : null,
        enacted_epoch: sample.status === "enacted" ? sample.epoch + 1 : null,
        expired_epoch: sample.status === "expired" ? sample.epoch : null,
        dropped_epoch: sample.status === "dropped" ? sample.epoch : null,
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
      else if (url.pathname === "/fixture-api/governance-actions")
        json = [action];
      else if (
        url.pathname === `/fixture-api/governance-actions/${sample.txHash}`
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
      `/governance_actions/history/${sample.txHash}#${sample.index}`
    );
    const panel = page.getByTestId("single-action-voting-numbers");
    await expect(panel).toBeVisible();
    await expect(panel).toContainText(
      {
        live: "In Progress",
        ratified: "Ratified",
        enacted: "Enacted",
        expired: "Expired",
        dropped: "Not Ratified",
      }[sample.status]
    );
    for (const [role, prefix] of [
      ["drep", "DReps"],
      ["spo", "SPOs"],
      ["cc", "CC"],
    ]) {
      const section = page.getByTestId(`${prefix}-voting-results-data`);
      const aggregate = sample.koios.find((a) => a.role === role);
      if (aggregate && sample.inconsistentRole !== role) {
        if (aggregate.totalEligible === aggregate.abstain) {
          await expect(section.getByRole("progressbar")).toHaveCount(0);
          await expect(section).toContainText(
            "There are no eligible non-abstaining votes"
          );
        } else {
          await expect(section.getByRole("progressbar")).toBeVisible();
        }
        const title = role === "cc" ? "Constitutional Committee" : prefix;
        await section.getByTestId(`${title}-expand-button`).click();
        const value =
          aggregate.representation === "percent"
            ? `${(Number(aggregate.yes) * 100).toFixed(2)}%`
            : aggregate.representation === "stake"
              ? `₳ ${((BigInt(aggregate.yes) + 999999n) / 1000000n).toLocaleString("en-US")}`
              : BigInt(aggregate.yes).toLocaleString("en-US");
        await expect(section.getByTestId(`${title}-yes-votes`)).toHaveText(
          value
        );
      } else {
        await expect(section.getByRole("status")).toBeVisible();
        await expect(section.getByRole("progressbar")).toHaveCount(0);
        if (sample.inconsistentRole === role) {
          await expect(section.getByRole("status")).toContainText(
            "inconsistent voting breakdown"
          );
        }
        await expect(
          page.getByTestId(`${prefix}-voting-results-indicator`)
        ).toContainText("-");
      }
      if (type === "InfoAction") {
        await expect(
          page.getByTestId(`${prefix}-voting-results-indicator`)
        ).toContainText("-");
      }
      if (aggregate?.passing === false && type !== "InfoAction") {
        const indicator = page.getByTestId(
          `${prefix}-voting-results-indicator`
        );
        await expect(indicator).not.toContainText("-");
        await expect(indicator.getByTestId("vote-result-icon")).toHaveCSS(
          "background-color",
          "rgb(211, 47, 47)"
        );
      }
    }
    if (sample === samples[0]) {
      await page.goto("/governance_actions/history");
      await expect(
        page.getByRole("heading", { name: "Governance action history" })
      ).toBeVisible();
      const details = page.getByTestId(
        `${sample.txHash}#${sample.index}-view-details`
      );
      await expect(details).toHaveAttribute(
        "href",
        `/governance_actions/history/${sample.txHash}#${sample.index}`
      );
      await details.click();
      await expect(
        page.getByTestId("single-action-voting-numbers")
      ).toBeVisible();
    }
    expect(networkMetricsRequests).toBe(0);
    expect(errors).toEqual([]);
  });
}
