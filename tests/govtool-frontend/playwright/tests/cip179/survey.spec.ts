import { test, expect } from "@playwright/test";
import { blake2b } from "blakejs";
import { Decoder } from "cbor-x";
import {
  QUICKNET_CHAIN_HASH,
  decryptWithBeacon,
} from "../../../../../govtool/frontend/node_modules/cip-179/dist/tlock/index.js";
import {
  setup,
  openVote,
  submitVote,
  checkTransaction,
  definition,
  questions,
  envelope,
  actionHash,
} from "./fixtures";

test("179-2A: public survey answers survive wallet signing", async ({
  page,
}) => {
  const fixture = await setup(page, { enabled: "default" });
  await openVote(page);
  await page.getByRole("checkbox", { name: /Include a survey/ }).check();
  await page.getByRole("radio", { name: "Usability", exact: true }).check();
  const slider = page.getByRole("slider", { name: "Choose a percentage" });
  await expect(slider).toHaveAttribute("aria-valuetext", "Not answered");
  await slider.press("Home");
  await slider.press("ArrowRight");
  await expect(slider).toHaveAttribute("aria-valuetext", "1");
  await submitVote(page);
  await expect.poll(() => fixture.transactions.signed.length).toBe(1);
  expect(checkTransaction(fixture.transactions)).toEqual([
    [1, 0, 0],
    [4, 1, 1],
  ]);
  expect(fixture.errors).toEqual([]);
});

test("179-2F: sealed responses encrypt the selected answers before signing", async ({
  page,
}) => {
  const fixture = await setup(page, {
    survey: definition({
      submissionMode: {
        type: "sealed",
        chainHash: QUICKNET_CHAIN_HASH,
        round: 1000,
        paddingSize: 64,
      },
    }),
  });
  await openVote(page);
  await page.getByRole("checkbox", { name: /Include a survey/ }).check();
  await page.getByRole("radio", { name: "Usability", exact: true }).check();
  await submitVote(page);
  await expect
    .poll(() => fixture.transactions.signed.length, { timeout: 20_000 })
    .toBe(1);
  const chunks = checkTransaction(fixture.transactions);
  const ciphertext = Buffer.concat(Array.isArray(chunks) ? chunks : [chunks]);
  expect(ciphertext.toString("utf8")).toContain("age-encryption.org/v1");
  // Public historical quicknet beacon: https://api.drand.sh/52db9ba70e0cc0f6eaf7803dd07447a1f5477735fd3f661792ba94600c84e971/public/1000
  // Past round permits offline verification, not confidentiality for a real survey.
  const plaintext = await decryptWithBeacon(ciphertext, {
    round: 1000,
    randomness:
      "fe290beca10872ef2fb164d2aa4442de4566183ec51c56ff3cd603d930e54fdd",
    signature:
      "b44679b9a59af2ec876b1a6b1ad52ea9b1615fc3982b19576350f93447cb1125e342b73a8dd2bacbe47e4b6b63ed5e39",
  });
  expect(new Decoder().decodeMultiple(plaintext)[0]).toEqual([[1, 0, 0]]);
  expect(plaintext.length).toBe(64);
  expect(fixture.errors).toEqual([]);
});

for (const mode of [
  "verified",
  "hash mismatch",
  "invalid fields",
  "unavailable",
] as const) {
  test(`179-3E: external presentation ${mode}`, async ({ page }) => {
    const body = JSON.stringify({
      specVersion: 5,
      kind: "cardano-survey-presentation",
      title: "External survey",
      questions: [
        {
          prompt: mode === "invalid fields" ? {} : "External question",
          options: ["External A", "External B"],
        },
      ],
    });
    const fixture = await setup(page, {
      survey: definition({
        title: "",
        questions: [
          {
            type: "singleChoice",
            prompt: "",
            options: { type: "count", count: 2 },
            required: true,
          },
        ],
        contentAnchor: {
          uri: "https://fixture.invalid/presentation.json",
          hash:
            mode === "hash mismatch"
              ? new Uint8Array(32)
              : blake2b(body, undefined, 32),
        },
      }),
      presentation: { body, status: mode === "unavailable" ? 404 : 200 },
    });
    await openVote(page);
    if (mode === "verified")
      await expect(
        page.getByRole("heading", { name: "External survey", exact: true })
      ).toBeVisible();
    else
      await expect(
        page.getByText(/External presentation unavailable:/)
      ).toBeVisible();
    await page.getByRole("checkbox", { name: /Include a survey/ }).check();
    await page
      .getByRole("radio", {
        name: mode === "verified" ? "External A" : "Option 1",
        exact: true,
      })
      .check();
    await submitVote(page);
    await expect.poll(() => fixture.transactions.signed.length).toBe(1);
    expect(checkTransaction(fixture.transactions)).toEqual([[1, 0, 0]]);
    expect(fixture.errors).toEqual([]);
  });
}

test("179-3F: unsupported optional custom methods can be omitted", async ({
  page,
}) => {
  const fixture = await setup(page, {
    survey: definition({
      questions: [
        questions[0],
        {
          type: "custom",
          prompt: "Custom method",
          methodSchema: {
            uri: "https://fixture.invalid/schema",
            hash: new Uint8Array(32),
          },
        },
      ],
    }),
  });
  await openVote(page);
  await page.getByRole("checkbox", { name: /Include a survey/ }).check();
  await expect(
    page.getByText("This custom survey method is not supported by GovTool.")
  ).toBeVisible();
  await page.getByRole("radio", { name: "Usability", exact: true }).check();
  await submitVote(page);
  await expect.poll(() => fixture.transactions.signed.length).toBe(1);
  expect(checkTransaction(fixture.transactions)).toEqual([[1, 0, 0]]);
});

for (const [name, options] of [
  ["no linked survey", { linked: false }],
  ["explicitly disabled", { enabled: false }],
  ["participation declined", {}],
] as const) {
  test(`179-2B: ordinary voting is preserved with ${name}`, async ({
    page,
  }) => {
    const fixture = await setup(page, options);
    await openVote(page);
    if (name === "participation declined") {
      await expect(
        page.getByRole("checkbox", { name: /Include a survey/ })
      ).not.toBeChecked();
    } else {
      await expect(
        page.getByRole("checkbox", { name: /Include a survey/ })
      ).toHaveCount(0);
    }
    await submitVote(page);
    await expect.poll(() => fixture.transactions.signed.length).toBe(1);
    expect(checkTransaction(fixture.transactions)).toBeUndefined();
    if (name !== "participation declined")
      expect(fixture.requests.some((p) => p.includes("/survey/"))).toBe(false);
    expect(fixture.errors).toEqual([]);
  });
}

test("179-2C: required answers block signing; clearing optional answers omits them", async ({
  page,
}) => {
  const fixture = await setup(page);
  await openVote(page);
  await page.getByRole("checkbox", { name: /Include a survey/ }).check();
  await page.getByRole("slider").press("Home");
  await submitVote(page);
  await expect(page.getByTestId("vote-context-modal")).toBeVisible();
  expect(fixture.transactions.unsigned).toHaveLength(0);
  await page.getByTestId("cancel-modal-button").click();
  await page.getByRole("radio", { name: "Usability", exact: true }).check();
  await page
    .getByRole("button", {
      name: "Clear answer: Choose a percentage",
      exact: true,
    })
    .click();
  await submitVote(page);
  await expect.poll(() => fixture.transactions.signed.length).toBe(1);
  expect(checkTransaction(fixture.transactions)).toEqual([[1, 0, 0]]);
  expect(fixture.errors).toEqual([]);
});

test("179-2D: numeric controls respect bounds, steps and explicit zero", async ({
  page,
}) => {
  const fixture = await setup(page, {
    survey: definition({
      questions: [
        ...questions,
        {
          type: "numericRange",
          prompt: "Signed steps",
          constraints: { min: -5n, max: 4n, step: 2n },
        },
        {
          type: "numericRange",
          prompt: "Fixed value",
          constraints: { min: 7n, max: 7n },
        },
        {
          type: "numericRange",
          prompt: "Large integer",
          constraints: { min: 0n, max: 18446744073709551615n },
        },
      ],
    }),
  });
  await openVote(page);
  await page.getByRole("checkbox", { name: /Include a survey/ }).check();
  await page.getByRole("radio", { name: "Usability", exact: true }).check();
  await expect(page.getByRole("textbox")).toHaveCount(0);
  const percent = page.getByRole("slider", { name: "Choose a percentage" });
  await percent.press("End");
  await percent.press("ArrowRight");
  await expect(percent).toHaveAttribute("aria-valuetext", "50");
  await percent.press("Home");
  await percent.press("ArrowLeft");
  await expect(percent).toHaveAttribute("aria-valuetext", "0");
  const signed = page.getByRole("slider", { name: "Signed steps" });
  await signed.press("End");
  await expect(signed).toHaveAttribute("aria-valuetext", "3");
  await page
    .getByRole("button", { name: "Select 7: Fixed value", exact: true })
    .click();
  await page
    .getByRole("button", { name: "Maximum: Large integer", exact: true })
    .click();
  await page
    .getByRole("button", { name: "Decrease: Large integer", exact: true })
    .click();
  await submitVote(page);
  await expect.poll(() => fixture.transactions.signed.length).toBe(1);
  expect(checkTransaction(fixture.transactions)).toEqual([
    [1, 0, 0],
    [4, 1, 0],
    [4, 2, 3],
    [4, 3, 7],
    [4, 4, 18446744073709551614n],
  ]);
  expect(fixture.errors).toEqual([]);
});

test("179-2E: selection, ranking, rating and allocation retain their answer types", async ({
  page,
}) => {
  const opts = (labels: string[]) => ({ type: "options" as const, labels });
  const fixture = await setup(page, {
    survey: definition({
      questions: [
        questions[0],
        {
          type: "multiSelect",
          prompt: "Select topics",
          options: opts(["Topic A", "Topic B"]),
          minSelections: 1,
          maxSelections: 1,
        },
        {
          type: "ranking",
          prompt: "Rank topics",
          options: opts(["Rank A", "Rank B"]),
          minRanked: 1,
          maxRanked: 2,
        },
        {
          type: "rating",
          prompt: "Numeric rating",
          options: opts(["Rate A", "Rate B"]),
          scale: { type: "numeric", constraints: { min: 0n, max: 5n } },
          requireAll: false,
        },
        {
          type: "rating",
          prompt: "Label rating",
          options: opts(["Guide", "Tutorial"]),
          scale: { type: "labels", labels: ["Poor", "Useful"] },
          requireAll: false,
        },
        {
          type: "pointsAllocation",
          prompt: "Allocate ten points",
          options: opts(["Outreach", "Testing"]),
          budget: 10,
        },
      ],
    }),
  });
  await openVote(page);
  await page.getByRole("checkbox", { name: /Include a survey/ }).check();
  await page.getByRole("radio", { name: "Usability", exact: true }).check();
  await page.getByRole("checkbox", { name: "Topic A", exact: true }).check();
  await page.getByRole("checkbox", { name: "Topic B", exact: true }).check();
  await expect(
    page.getByText("Complete the selected survey response before submitting.")
  ).toBeVisible();
  await page.getByRole("checkbox", { name: "Topic B", exact: true }).uncheck();
  await page.getByRole("checkbox", { name: "Rank B", exact: true }).check();
  await page.getByRole("checkbox", { name: "Rank A", exact: true }).check();
  await page.getByRole("slider", { name: "Rate A", exact: true }).press("Home");
  await page.getByRole("combobox", { name: "Guide", exact: true }).click();
  await page.getByRole("option", { name: "Useful", exact: true }).click();
  await page
    .getByRole("spinbutton", { name: "Outreach", exact: true })
    .fill("4");
  await expect(
    page.getByText("Complete the selected survey response before submitting.")
  ).toBeVisible();
  await page
    .getByRole("spinbutton", { name: "Testing", exact: true })
    .fill("6");
  await expect(
    page.getByText("Complete the selected survey response before submitting.")
  ).toHaveCount(0);
  await submitVote(page);
  await expect.poll(() => fixture.transactions.signed.length).toBe(1);
  expect(checkTransaction(fixture.transactions)).toEqual([
    [1, 0, 0],
    [2, 1, [0]],
    [3, 2, [1, 0]],
    [6, 3, [[0, 0]]],
    [6, 4, [[0, 1]]],
    [
      5,
      5,
      [
        [0, 4],
        [1, 6],
      ],
    ],
  ]);
  expect(fixture.errors).toEqual([]);
});

for (const [name, options] of [
  ["unavailable definition", { surveyStatus: 404 }],
  ["wrong reference", { envelope: { ...envelope(), txId: "cd".repeat(32) } }],
  ["wrong expiry", { survey: definition({ endEpoch: 529 }) }],
  [
    "oversized survey",
    {
      survey: definition({
        questions: Array.from({ length: 101 }, () => questions[0]),
      }),
    },
  ],
] as const) {
  test(`179-3A: ${name} does not break ordinary voting`, async ({ page }) => {
    const fixture = await setup(page, options);
    await openVote(page);
    await expect(
      page.getByText(/Linked survey response unavailable:/)
    ).toBeVisible();
    await expect(
      page.getByRole("checkbox", { name: /Include a survey/ })
    ).toHaveCount(0);
    await submitVote(page);
    await expect.poll(() => fixture.transactions.signed.length).toBe(1);
    expect(checkTransaction(fixture.transactions)).toBeUndefined();
    expect(fixture.errors).toEqual([]);
  });
}

test("179-3B: ineligible roles cannot attach a survey response", async ({
  page,
}) => {
  const fixture = await setup(page, {
    survey: definition({ eligibleRoles: [1] }),
  });
  await openVote(page);
  await expect(page.getByText(/does not accept DRep responses/)).toBeVisible();
  await expect(
    page.getByRole("checkbox", { name: /Include a survey/ })
  ).toHaveCount(0);
  await submitVote(page);
  await expect.poll(() => fixture.transactions.signed.length).toBe(1);
  expect(checkTransaction(fixture.transactions)).toBeUndefined();
});

test("179-3C: declining wallet signing never submits a transaction", async ({
  page,
}) => {
  const fixture = await setup(page, { rejectSigning: true });
  await openVote(page);
  await page.getByRole("checkbox", { name: /Include a survey/ }).check();
  await page.getByRole("radio", { name: "Usability", exact: true }).check();
  await submitVote(page);
  await expect(page.getByTestId("vote-transaction-error-modal")).toBeVisible();
  expect(fixture.transactions.unsigned).toHaveLength(1);
  expect(fixture.transactions.signed).toHaveLength(0);
});

for (const connected of [false, true]) {
  test(`179-3D: ${connected ? "non-DRep" : "disconnected"} users cannot sign survey votes`, async ({
    page,
  }) => {
    const fixture = await setup(page, { connected, registered: false });
    await page.goto(
      `/${connected ? "connected/" : ""}governance_actions/${actionHash}#0`
    );
    await expect(
      page.getByRole("heading", {
        name: "Survey-linked Info Action",
        exact: true,
      })
    ).toBeVisible();
    await expect(page.getByTestId("vote-button")).toHaveCount(0);
    expect(fixture.transactions.unsigned).toHaveLength(0);
  });
}
