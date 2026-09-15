import { test, expect, Page } from "@playwright/test";
import { readFile } from "node:fs/promises";
import { setup, surveyHash } from "./fixtures";

async function openForm(page: Page) {
  await page.goto("/create_governance_action");
  await page.getByTestId("continue-button").click();
  await page.getByTestId("InfoAction-radio").click();
  await page.getByTestId("continue-button").click();
  for (const [name, value] of Object.entries({
    title: "Test Info Action",
    abstract: "Abstract text",
    motivation: "Motivation text",
    rationale: "Rationale text",
  })) {
    await page.locator(`[name="${name}"]`).fill(value);
  }
}

for (const linked of [true, false]) {
  test(`179-1A: authoring ${linked ? "with" : "without"} a survey preserves CIP-108 body`, async ({
    page,
  }) => {
    const fixture = await setup(page, { enabled: "default" });
    await openForm(page);
    if (linked)
      await page.locator('[name="surveyTxId"]').fill(surveyHash.toUpperCase());
    await page.getByTestId("continue-button").click();
    await page.getByTestId("continue-button").click();
    await page
      .getByTestId("storing-information-checkbox")
      .getByRole("checkbox")
      .check();
    await page.getByTestId("continue-button").click();
    await expect(page.getByTestId("metadata-download-button")).toBeVisible();
    const downloaded = page.waitForEvent("download");
    await page.getByTestId("metadata-download-button").click();
    const file = await (await downloaded).path();
    expect(file).not.toBeNull();
    const json = JSON.parse(await readFile(file!, "utf8"));
    expect(json.body).toMatchObject({
      title: "Test Info Action",
      abstract: "Abstract text",
      motivation: "Motivation text",
      rationale: "Rationale text",
    });
    if (linked)
      expect(json.body.cip179).toEqual({
        specVersion: 5,
        kind: "survey-link",
        surveyTxId: surveyHash,
        surveyIndex: 0,
      });
    else {
      expect(json.body.cip179).toBeUndefined();
      expect(json["@context"].CIP179).toBeUndefined();
      expect(fixture.requests.some((p) => p.includes("/survey/"))).toBe(false);
    }
    expect(JSON.stringify(json)).not.toContain("Choose a percentage");
    expect(fixture.transactions.unsigned).toHaveLength(0);
    expect(fixture.errors).toEqual([]);
  });
}

test("179-1B: malformed survey transaction IDs cannot proceed", async ({
  page,
}) => {
  const fixture = await setup(page);
  await openForm(page);
  await page.locator('[name="surveyTxId"]').fill("not-a-transaction-hash");
  await expect(page.getByTestId("continue-button")).toBeDisabled();
  await page.locator('[name="surveyTxId"]').fill("");
  await expect(page.getByTestId("continue-button")).toBeEnabled();
  expect(fixture.requests.some((p) => p.includes("/survey/"))).toBe(false);
});

test("179-1C: explicit feature opt-out hides the survey authoring field", async ({
  page,
}) => {
  await setup(page, { enabled: false });
  await openForm(page);
  await expect(page.locator('[name="surveyTxId"]')).toHaveCount(0);
  await expect(page.getByTestId("continue-button")).toBeEnabled();
});
