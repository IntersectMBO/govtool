import { GOVERNANCE_ACTION_CONTEXT_WITH_CIP179 } from "@consts";
import { generateJsonld } from "@utils";

describe("CIP-179 governance-action authoring", () => {
  it("keeps the complete survey link inside the witnessed CIP-108 body", async () => {
    const surveyTxId = "ab".repeat(32);
    const json = await generateJsonld(
      {
        title: "Survey-linked action",
        abstract: "Abstract",
        motivation: "Motivation",
        rationale: "Rationale",
        cip179: {
          specVersion: 5,
          kind: "survey-link",
          surveyTxId,
          surveyIndex: 0,
        },
      },
      GOVERNANCE_ACTION_CONTEXT_WITH_CIP179,
    );

    const body = json.body as Record<string, unknown>;
    const context = json["@context"] as Record<string, unknown>;
    expect(body.cip179).toMatchObject({
      specVersion: 5,
      kind: "survey-link",
      surveyTxId,
      surveyIndex: 0,
    });
    expect(context.CIP179).toContain("CIP-0179");
  });
});
