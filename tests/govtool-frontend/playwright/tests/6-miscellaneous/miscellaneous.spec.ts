import {
  BOOTSTRAP_DOC_URL,
  DELEGATION_DOC_URL,
  DIRECT_VOTER_DOC_URL,
  FAQS_DOC_URL,
  GUIDES_DOC_URL,
  HELP_DOC_URL,
  PRIVACY_POLICY,
  REGISTER_DREP_DOC_URL,
  TERMS_AND_CONDITIONS,
} from "@constants/docsUrl";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import { expect, Page } from "@playwright/test";
import { randomUUID } from "crypto";
import environments from "lib/constants/environments";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});

test("6C. Navigation within the dApp", async ({ page, context }) => {
  await page.goto("/");

  if (isMobile(page)) {
    await openDrawer(page);
  }
  await page.getByTestId("governance-actions-link").click();
  await expect(page).toHaveURL(/\/governance_actions/);

  if (isMobile(page)) {
    await openDrawer(page);
  }
  const [guidesPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("guides-link").click(),
  ]);

  await expect(guidesPage).toHaveURL(GUIDES_DOC_URL);

  if (isMobile(page)) {
    await openDrawer(page);
  }
  const [faqsPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("faqs-link").click(),
  ]);

  await expect(faqsPage).toHaveURL(FAQS_DOC_URL);

  if (isMobile(page)) {
    await openDrawer(page);
  }
  await page.getByTestId("dashboard-link").click();
  expect(page.url()).toEqual(`${environments.frontendUrl}/`);
});

test("6D. Should open Sanchonet docs in a new tab when clicking `Learn More` on dashboards in disconnected state.", async ({
  page,
  context,
}) => {
  await page.goto("/");

  const [delegationLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("delegate-learn-more-button").click(),
  ]);
  await expect(delegationLearnMorepage).toHaveURL(DELEGATION_DOC_URL);

  const [registerLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("d-rep-learn-more-button").click(),
  ]);
  await expect(registerLearnMorepage).toHaveURL(REGISTER_DREP_DOC_URL);

  const [directVoterLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("direct-voter-learn-more-button").click(),
  ]);
  await expect(directVoterLearnMorepage).toHaveURL(DIRECT_VOTER_DOC_URL);
});

test("6M. Should navigate between footer links", async ({ page, context }) => {
  await page.goto("/");

  const [privacyPolicy] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("privacy-policy-footer-link").click(),
  ]);
  await expect(privacyPolicy).toHaveURL(PRIVACY_POLICY);

  const [termsAndConditions] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("terms-of-use-footer-link").click(),
  ]);
  await expect(termsAndConditions).toHaveURL(TERMS_AND_CONDITIONS);

  const [helpUrl] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("help-footer-button").click(),
  ]);
  await expect(helpUrl).toHaveURL(HELP_DOC_URL);
});

test.describe("User Snap", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
    await page.waitForTimeout(2_000); // wait until page load properly

    await page.getByTestId("feedback-footer-button").click();
  });

  test("6N. Should open feedback modal", async ({ page }) => {
    await expect(page.getByLabel("Usersnap widget")).toBeVisible();
    await expect(
      page.getByRole("button", {
        name: "Report an issue Something",
      })
    ).toBeVisible();
    await expect(
      page.getByRole("button", {
        name: "Idea or new feature Let us",
      })
    ).toBeVisible();
  });

  test("6O. Should verify a bug report form", async ({ page }) => {
    await page
      .getByRole("button", {
        name: "Report an issue Something",
      })
      .click();

    await expect(
      page.getByRole("heading", { name: "Report a bug" })
    ).toBeVisible();
    await expect(page.getByPlaceholder("Your feedback")).toBeVisible();
    await expect(page.getByText("Drag & drop or Browse")).toBeVisible();
    await expect(page.getByLabel("Take screenshot")).toBeVisible();
    await expect(page.getByLabel("Record")).toBeVisible();
    await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
  });

  test("6P. Should verify feature form", async ({ page }) => {
    await page
      .getByRole("button", {
        name: "Idea or new feature Let us",
      })
      .click();

    await expect(
      page.getByRole("heading", { name: "Idea or new feature" })
    ).toBeVisible();
    await expect(
      page.getByPlaceholder("Example: New navigation")
    ).toBeVisible();
    await expect(
      page.getByPlaceholder("Example: New navigation")
    ).toBeVisible();
    await expect(page.getByLabel("Any additional details")).toBeVisible();
    await expect(page.getByText("Drag & drop or Browse")).toBeVisible();
    await expect(
      page.getByLabel("Please summarize your idea or")
    ).toBeVisible();
    await expect(page.getByLabel("Take screenshot")).toBeVisible();
    await expect(page.getByLabel("Record")).toBeVisible();
    await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
  });

  test.describe("Feedback Tests", () => {
    const attachmentInputSelector = "input[type=file]";
    const feedbackApiUrl =
      "https://widget.usersnap.com/api/widget/xhrrpc?submit_feedback";
    const bucketUrl =
      "https://s3.eu-central-1.amazonaws.com/upload.usersnap.com";
    const mockAttachmentPath = "./lib/_mock/mockAttachment.png";

    const interceptBucket = async (page: Page) => {
      await page.route(bucketUrl, async (route) =>
        route.fulfill({
          status: 204,
        })
      );
    };

    const interceptUsersnap = async (page: Page) => {
      await page.route(feedbackApiUrl, async (route) =>
        route.fulfill({
          status: 200,
          body: JSON.stringify({
            status: true,
            data: {
              feedback: {
                feedback_id: randomUUID(),
                assignee_id: randomUUID(),
                labels: [],
              },
              screen_recording_url: null,
              attachment_urls: [
                {
                  url: bucketUrl,
                  fields: {
                    "Content-Type": "image/png",
                    key: randomUUID(),
                    "x-amz-algorithm": "AWS4-HMAC-SHA256",
                    "x-amz-credential":
                      "ASIAQL7JRYTOEKCBSNV5/20250211/eu-central-1/s3/aws4_request",
                    "x-amz-date": "20250211T041923Z",
                    "x-amz-security-token":
                      "IQoJb3JpZ2luX2VjELT//////////wEaDGV1LWNlbnRyYWwtMSJHMEUCIBxFIrw90W0SUrmqdkLwJj9CPik/Zdp/IbpnZAzNQ88hAiEAxzNolOJbTyUDY/8jUBruYe41G2/hVVZ5Klhzjrc9utcq4QQIzf//////////ARAAGgwwMjU3MjI3MzM3ODgiDKC6DC3BTvvNZGmdYCq1BBCGKULckA+wyt7ByvNgfMEGjhxF5pM4EAGWp5t/t4X4LEK6Atf+LQ5LGL4ZdpLfIIqUzIGGdjDf17rKPqtI4W9XgMceCFVBaZGQJGdX9IjhHQs/rd+Vdn3yPVz0uoHPFqBIWw5ErEa8U/R9UxO2ayusznotuCsLKPMg3GsglPSEZrBm5lmppYxUPCRp2L2tr7tFWTCTV1tv5/KrUeSl41scZWz0dwcIV5skNuVY0K+LxqLunld9X0/i3BPllW3CTv+v5Qz020L6z4JGFhWgP/qLudX3D2VI34d6aO8a+hjObXFWp46yT1Gb69D3PDaa/ee9jO9nc6vIjsk3I5eK+Zte26M0NHOSXnueCiJAgJ4VaEAVR0KjVl8a9t3pOlWzt7qP3XL7wc8/cRREULY1BAm8TRjcVJD4I/udKjMuU5dBIyBPZ29D5AQrgtsbFNkokGCWah5lCjLA1Wo2KwJklWvGR/fislBH27ybcMK9t1PEZxDwZuad5uxlfa0ZTDw4a+jE5xEHwXQUyCfS10n19h8E6tlfrMfTjDAfxcKwjeMKV+gNg/fKsEhk7RWwrrdm74zGlBsGut93pJ7VWpEopdZSrzNniaRIjSmS6u5NgNHmQl+rN4wX9tjm8Q9PacuVvx2lV4WlkpJ/sd7Y02RsXqPiAsc1hsXa7sDxaLn7CdQd+nveObYjqLYu3U2S1BVxpDujFOqdxeUziHE3NJexqmsue6LYNTIKCDwDvL5gRbD8H26jylwwqZOrvQY6pwHLPFojxcR61xtvWxFpYxewobGXGVCBJWHikZWASmmmO+OL3PdoVKu0xOks0Dd6DvkZhYTaxlg/ER77nyapOTOPMWBCxVJlQ3iCs3NEFOznhj8akqEsTo/U8/uII3RX1iGWQdm7VgZUH5kgTOdPV3NdMNoahja2c7qMRZBoNPXDnREYnwaOBSbVSmnGIX9Gq4eNa7N1nNsCgaU2JtfS8qUjZy9OKiW69Q==",
                    policy:
                      "eyJleHBpcmF0aW9uIjogIjIwMjUtMDItMTFUMDQ6MjQ6MjNaIiwgImNvbmRpdGlvbnMiOiBbWyJjb250ZW50LWxlbmd0aC1yYW5nZSIsIDEsIDIwOTcxNTIwMF0sIFsiZXEiLCAiJENvbnRlbnQtVHlwZSIsICJpbWFnZS9qcGVnIl0sIHsiYnVja2V0IjogInVwbG9hZC51c2Vyc25hcC5jb20ifSwgeyJrZXkiOiAiMGRkZDJkODgtYmJkNy00MWE1LWFmOWItMjBlNWI3N2Q4ZWQ2In0sIHsieC1hbXotYWxnb3JpdGhtIjogIkFXUzQtSE1BQy1TSEEyNTYifSwgeyJ4LWFtei1jcmVkZW50aWFsIjogIkFTSUFRTDdKUllUT0VLQ0JTTlY1LzIwMjUwMjExL2V1LWNlbnRyYWwtMS9zMy9hd3M0X3JlcXVlc3QifSwgeyJ4LWFtei1kYXRlIjogIjIwMjUwMjExVDA0MTkyM1oifSwgeyJ4LWFtei1zZWN1cml0eS10b2tlbiI6ICJJUW9KYjNKcFoybHVYMlZqRUxULy8vLy8vLy8vL3dFYURHVjFMV05sYm5SeVlXd3RNU0pITUVVQ0lCeEZJcnc5MFcwU1VybXFka0x3Smo5Q1Bpay9aZHAvSWJwblpBek5RODhoQWlFQXh6Tm9sT0piVHlVRFkvOGpVQnJ1WWU0MUcyL2hWVlo1S2xoempyYzl1dGNxNFFRSXpmLy8vLy8vLy8vL0FSQUFHZ3d3TWpVM01qSTNNek0zT0RnaURLQzZEQzNCVHZ2TlpHbWRZQ3ExQkJDR0tVTGNrQSt3eXQ3Qnl2TmdmTUVHamh4RjVwTTRFQUdXcDV0L3Q0WDRMRUs2QXRmK0xRNUxHTDRaZHBMZklJcVV6SUdHZGpEZjE3cktQcXRJNFc5WGdNY2VDRlZCYVpHUUpHZFg5SWpoSFFzL3JkK1ZkbjN5UFZ6MHVvSFBGcUJJV3c1RXJFYThVL1I5VXhPMmF5dXN6bm90dUNzTEtQTWczR3NnbFBTRVpyQm01bG1wcFl4VVBDUnAyTDJ0cjd0RldUQ1RWMXR2NS9LclVlU2w0MXNjWld6MGR3Y0lWNXNrTnVWWTBLK0x4cUx1bmxkOVgwL2kzQlBsbFczQ1R2K3Y1UXowMjBMNno0SkdGaFdnUC9xTHVkWDNEMlZJMzRkNmFPOGEraGpPYlhGV3A0NnlUMUdiNjlEM1BEYWEvZWU5ak85bmM2dklqc2szSTVlSytadGUyNk0wTkhPU1hudWVDaUpBZ0o0VmFFQVZSMEtqVmw4YTl0M3BPbFd6dDdxUDNYTDd3YzgvY1JSRVVMWTFCQW04VFJqY1ZKRDRJL3VkS2pNdVU1ZEJJeUJQWjI5RDVBUXJndHNiRk5rb2tHQ1dhaDVsQ2pMQTFXbzJLd0prbFd2R1IvZmlzbEJIMjd5YmNNSzl0MVBFWnhEd1p1YWQ1dXhsZmEwWlREdzRhK2pFNXhFSHdYUVV5Q2ZTMTBuMTloOEU2dGxmck1mVGpEQWZ4Y0t3amVNS1YrZ05nL2ZLc0VoazdSV3dycmRtNzR6R2xCc0d1dDkzcEo3VldwRW9wZFpTcnpObmlhUklqU21TNnU1TmdOSG1RbCtyTjR3WDl0am04UTlQYWN1VnZ4MmxWNFdsa3BKL3NkN1kwMlJzWHFQaUFzYzFoc1hhN3NEeGFMbjdDZFFkK252ZU9iWWpxTFl1M1UyUzFCVnhwRHVqRk9xZHhlVXppSEUzTkpleHFtc3VlNkxZTlRJS0NEd0R2TDVnUmJEOEgyNmp5bHd3cVpPcnZRWTZwd0hMUEZvanhjUjYxeHR2V3hGcFl4ZXdvYkdYR1ZDQkpXSGlrWldBU21tbU8rT0wzUGRvVkt1MHhPa3MwRGQ2RHZrWmhZVGF4bGcvRVI3N255YXBPVE9QTVdCQ3hWSmxRM2lDczNORUZPem5oajhha3FFc1RvL1U4L3VJSTNSWDFpR1dRZG03VmdaVUg1a2dUT2RQVjNOZE1Ob2FoamEyYzdxTVJaQm9OUFhEblJFWW53YU9CU2JWU21uR0lYOUdxNGVOYTdOMW5Oc0NnYVUySnRmUzhxVWpaeTlPS2lXNjlRPT0ifV19",
                    "x-amz-signature":
                      "b3eb39a9084457b61fc99d1c8ec4cb0170371aaa6327500eafae23dcb4abecb5",
                  },
                },
              ],
            },
          }),
        })
      );
    };

    test("6Q. Should report an issue", async ({ page }) => {
      // Intercept Usersnap submit API
      await interceptUsersnap(page);
      await interceptBucket(page);
      await page
        .getByRole("button", {
          name: "Report an issue Something",
        })
        .click();

      await page
        .getByPlaceholder("Your feedback")
        .fill(faker.lorem.paragraph(2));
      await page.setInputFiles(attachmentInputSelector, [mockAttachmentPath]);

      await page.getByRole("button", { name: "Submit" }).click();

      await expect(page.getByText("Thank you!")).toBeVisible();
    });

    test("6R. Should submit an idea or new feature", async ({ page }) => {
      // Intercept Usersnap submit API
      await interceptUsersnap(page);
      await interceptBucket(page);

      await page
        .getByRole("button", {
          name: "Idea or new feature Let us",
        })
        .click();

      await page
        .getByPlaceholder("Example: New navigation")
        .fill(faker.lorem.words(4));
      await page
        .getByLabel("Please summarize your idea or")
        .fill(faker.lorem.paragraph(2));
      await page
        .getByLabel("Any additional details")
        .fill(faker.lorem.paragraph(2));
      await page.setInputFiles(attachmentInputSelector, [mockAttachmentPath]);

      await page.getByRole("button", { name: "Submit" }).click();

      await expect(page.getByText("Thank you!")).toBeVisible();
    });
  });
});

test("6S. Should Warn users that they are in bootstrapping phase via banner", async ({
  page,
  context,
}) => {
  await page.route("**/epoch/params", async (route) => {
    // Fetch the original response from the server
    const response = await route.fetch();
    const json = await response.json();

    // update protocol major version
    json["protocol_major"] = 9;
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(json),
    });
  });

  const responsePromise = page.waitForResponse("**/epoch/params");
  await page.goto("/");

  await responsePromise;

  await expect(page.getByTestId("system-bootstrapping-warning")).toBeVisible();

  const [bootstrap] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("system-bootstrapping-warning-link").click(),
  ]);
  await expect(bootstrap).toHaveURL(BOOTSTRAP_DOC_URL);
});

test("6T. Should display proper network name", async ({ page }) => {
  await page.route("**/network/metrics", async (route) => {
    // Fetch the original response from the server
    const response = await route.fetch();
    const json = await response.json();

    const networkNames = ["sanchonet", "preview"];
    // update network name
    json["networkName"] =
      networkNames[Math.floor(Math.random() * networkNames.length)];
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(json),
    });
  });
  const responsePromise = page.waitForResponse("**/network/metrics");
  await page.goto("/");

  const response = await responsePromise;
  const responseBody = await response.json();

  await expect(page.getByTestId("system-network-name")).toHaveText(
    new RegExp(responseBody["networkName"], "i")
  );
});
