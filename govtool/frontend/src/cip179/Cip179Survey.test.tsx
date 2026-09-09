import { type ComponentProps } from "react";
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { encodePayload, type Question, type SurveyDefinition } from "cip-179";
import { API as client } from "@services";
import { Cip179Survey } from "./Cip179Survey";
import { metadatumCodec } from "./csl";

const txId = "ab".repeat(32);
const metadata = {
  body: {
    cip179: {
      specVersion: 5,
      kind: "survey-link",
      surveyTxId: txId,
      surveyIndex: 0,
    },
  },
};
const proposal = {
  expiryEpochNo: 501,
  json: metadata,
  metadataJson: metadata,
} as unknown as ComponentProps<typeof Cip179Survey>["proposal"];
const options = { type: "options" as const, labels: ["Extra", "Other"] };
const questions: Question[] = [
  { type: "numericRange", prompt: "Extra", constraints: { min: 0n, max: 5n } },
  { type: "ranking", prompt: "Rank", options, minRanked: 1, maxRanked: 1 },
  {
    type: "rating",
    prompt: "Rate",
    options,
    scale: { type: "numeric", constraints: { min: 0n, max: 5n } },
    requireAll: false,
  },
  {
    type: "multiSelect",
    prompt: "Select",
    options,
    minSelections: 0,
    maxSelections: 1,
  },
];

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});
it.each(questions)(
  "clears $type without losing required-answer or omission semantics",
  async (question) => {
    const definition: SurveyDefinition = {
      specVersion: 5,
      title: "Survey",
      description: "",
      owner: { type: "key", keyHash: new Uint8Array(28) },
      eligibleRoles: [0],
      endEpoch: 500,
      submissionMode: { type: "public" },
      questions: [
        {
          type: "singleChoice",
          prompt: "Choose",
          options: { type: "options", labels: ["Alpha", "Beta"] },
          required: true,
        },
        question,
      ],
    };
    const payload = encodePayload({
      type: "definitions",
      definitions: [definition],
    });
    const payloadCborHex = Buffer.from(
      metadatumCodec.metadatumToCbor(payload),
    ).toString("hex");
    vi.spyOn(client, "get").mockResolvedValue({
      data: { txId, surveyIndex: 0, metadataLabel: 17, payloadCborHex },
    });
    const changed = vi.fn<ComponentProps<typeof Cip179Survey>["onChange"]>();
    const state = () => changed.mock.calls[changed.mock.calls.length - 1][0];
    render(
      <Cip179Survey
        proposal={proposal}
        dRepId={"22".repeat(28)}
        onChange={changed}
      />,
    );
    await screen.findByText("Survey");
    expect(state()).toMatchObject({
      participating: false,
      valid: true,
      response: null,
    });
    fireEvent.click(screen.getByRole("checkbox", { name: /Include a survey/ }));
    expect(state().valid).toBe(false);
    fireEvent.click(screen.getByRole("radio", { name: "Alpha" }));
    expect(state().valid).toBe(true);
    const numeric = question.type === "numericRange" || question.type === "rating";
    const input = screen.getByRole(numeric ? "textbox" : "checkbox", { name: "Extra" });
    if (numeric) fireEvent.change(input, { target: { value: "0" } });
    else fireEvent.click(input);
    expect(state().valid).toBe(true);
    if (numeric) fireEvent.change(input, { target: { value: "" } });
    else fireEvent.click(input);
    expect(state().valid).toBe(true);
    expect(state().response?.answers).toMatchObject({
      answers:
        question.type === "multiSelect"
          ? [{ optionIndex: 0 }, { optionIndices: [] }]
          : [{ optionIndex: 0 }],
    });
    fireEvent.click(screen.getByRole("checkbox", { name: /Include a survey/ }));
    expect(state()).toMatchObject({
      participating: false,
      valid: true,
      response: null,
    });
  },
);
