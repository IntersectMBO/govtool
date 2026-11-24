import { describe, expect, it } from "vitest";

import i18n from "@/i18n";
import type { GovToolNotification } from "@/models";

import { describeNotification, safeHref } from "./notificationContent";

const t = i18n.t.bind(i18n);
const TX = "a".repeat(64);
// Built from parts: the point is that the app refuses it.
const SCRIPT_URL = ["javascript", "alert(1)"].join(":");

describe("describeNotification", () => {
  it("words a retired DRep with the voting power and both links", () => {
    const content = describeNotification(
      {
        id: "1",
        kind: "drepChange",
        createdAt: "2026-10-02T00:00:00Z",
        read: false,
        data: { dRepId: "drep1hazel", dRepName: "Hazel Pool", change: "retired", epoch: null, votingPower: 1_840_000_000_000 },
      } as GovToolNotification,
      t,
    );
    expect(content.title).toBe("Your DRep, Hazel Pool, has retired");
    expect(content.body).toContain("₳ 1.84M");
    expect(content.status).toEqual({ label: "Action needed", tone: "error" });
    expect(content.actions.map((action) => action.path)).toEqual([
      "/dreps",
      "/dreps/drep1hazel",
    ]);
  });

  it("links an outcome by its CIP-129 id, which survives a URL path", () => {
    const content = describeNotification(
      {
        id: "2",
        kind: "actionActivity",
        createdAt: "2026-10-02T00:00:00Z",
        read: false,
        data: { actionId: `${TX}#1`, actionType: "TreasuryWithdrawals", title: null, outcome: "ratified", vote: "yes", relation: "voted" },
      } as GovToolNotification,
      t,
    );
    expect(content.actions[0].path).toMatch(/^\/actions\/history\/gov_action1/);
    expect(content.body).toContain("Your recorded vote: Yes");
  });

  it("opens only http(s) links that come from the service", () => {
    expect(safeHref("https://gov.tools/x")).toBe("https://gov.tools/x");
    expect(safeHref(SCRIPT_URL)).toBeUndefined();
    const content = describeNotification(
      {
        id: "3",
        kind: "system",
        createdAt: "2026-10-02T00:00:00Z",
        read: false,
        data: { title: "T", body: "B", url: SCRIPT_URL },
      } as GovToolNotification,
      t,
    );
    expect(content.actions).toEqual([]);
  });
});
