import { type ComponentProps } from "react";
import { act, cleanup, fireEvent, render, screen } from "@testing-library/react";
import { encodePayload, type Question, type SurveyDefinition } from "cip-179";
import { API as client } from "@services";
import userEvent from "@testing-library/user-event";
import { buildAnswer, Cip179Survey, customMethodKey } from "./Cip179Survey";
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

async function renderSurvey(
  question: Question | Question[],
  customRenderers?: ComponentProps<typeof Cip179Survey>["customRenderers"],
) {
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
      ...(Array.isArray(question) ? question : [question]),
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
  const view = render(
    <Cip179Survey
      proposal={proposal}
      dRepId={"22".repeat(28)}
      onChange={changed}
      customRenderers={customRenderers}
    />,
  );
  await screen.findByText("Survey");
  return Object.assign(state, { reset: () => view.rerender(
    <Cip179Survey proposal={{ ...proposal, json: {} }} dRepId={"22".repeat(28)} onChange={changed} />,
  ) });
}

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});
it.each(questions)(
  "clears $type without losing required-answer or omission semantics",
  async (question) => {
    const state = await renderSurvey(question);
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
    const input = screen.getByRole(numeric ? "slider" : "checkbox", { name: "Extra" });
    if (numeric) fireEvent.keyDown(input, { key: "Home" });
    else fireEvent.click(input);
    expect(state().valid).toBe(true);
    if (numeric) fireEvent.click(screen.getByRole("button", { name: "Clear answer: Extra" }));
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

const numericQuestions: Question[] = [
  { type: "numericRange", prompt: "Extra", constraints: { min: 0n, max: 50n, step: 1n }, required: true },
  {
    type: "rating",
    prompt: "Rate",
    options,
    scale: { type: "numeric", constraints: { min: 0n, max: 50n, step: 1n } },
    requireAll: false,
    required: true,
  },
];

const withConstraints = (
  question: Question, constraints: { min: bigint; max: bigint; step?: bigint },
): Question => (
  question.type === "numericRange"
    ? { ...question, constraints }
    : { ...question as Extract<Question, { type: "rating" }>, scale: { type: "numeric", constraints } }
);

const participate = () => {
  fireEvent.click(screen.getByRole("checkbox", { name: /Include a survey/ }));
  fireEvent.click(screen.getByRole("radio", { name: "Alpha" }));
};

it.each(numericQuestions)("selects and clears $type without a text box or implicit answer", async (question) => {
  const state = await renderSurvey(question);
  participate();
  const slider = screen.getByRole("slider", { name: "Extra" });
  expect(screen.queryByRole("textbox")).not.toBeInTheDocument();
  fireEvent.focus(slider);
  expect(slider).toHaveAttribute("aria-valuetext", "Not answered");
  expect(state()).toMatchObject({ valid: false, response: null });
  const user = userEvent.setup();
  await act(() => user.type(slider, "abc"));
  expect(state().valid).toBe(false);
  ([["Home", 0n], ["ArrowRight", 1n], ["PageUp", 11n], ["End", 50n], ["ArrowRight", 50n]] as const).forEach(([key, value]) => {
    fireEvent.keyDown(slider, { key });
    expect(slider).toHaveAttribute("aria-valuetext", String(value));
    expect(state().valid).toBe(true);
    expect(state().response?.answers).toMatchObject({ answers: [
      { optionIndex: 0 },
      question.type === "rating" ? { ratings: [{ optionIndex: 0, rating: value }] } : { value },
    ] });
  });
  fireEvent.click(screen.getByRole("button", { name: "Clear answer: Extra" }));
  expect(state()).toMatchObject({ valid: false, response: null });
  expect(slider).toHaveAttribute("aria-valuetext", "Not answered");
  fireEvent.click(screen.getByRole("checkbox", { name: /Include a survey/ }));
  expect(state()).toMatchObject({ participating: false, valid: true, response: null });
});

it.each(numericQuestions)("keeps $type steps relative to a negative minimum and within an unaligned maximum", async (question) => {
  const state = await renderSurvey(withConstraints(question, { min: -5n, max: 4n, step: 2n }));
  participate();
  const slider = screen.getByRole("slider", { name: "Extra" });
  expect(slider).toHaveAccessibleDescription("-5 to 4, step 2");
  [["Home", "-5"], ["ArrowLeft", "-5"], ["ArrowRight", "-3"], ["End", "3"], ["ArrowUp", "3"]].forEach(([key, value]) => {
    fireEvent.keyDown(slider, { key });
    expect(slider).toHaveAttribute("aria-valuetext", value);
    expect(state().valid).toBe(true);
  });
});

it.each(numericQuestions)("preserves $type integers beyond Number precision", async (question) => {
  const min = 9007199254740993n;
  const state = await renderSurvey(withConstraints(question, { min, max: min + 4n, step: 2n }));
  participate();
  const slider = screen.getByRole("slider", { name: "Extra" });
  fireEvent.change(slider, { target: { value: "1" } });
  expect(slider).toHaveAttribute("aria-valuetext", String(min + 2n));
  expect(state().valid).toBe(true);
  expect(state().response?.answers).toMatchObject({ answers: [
    { optionIndex: 0 },
    question.type === "rating" ? { ratings: [{ rating: min + 2n }] } : { value: min + 2n },
  ] });
});

it.each(numericQuestions)("uses exact $type buttons for ranges too wide for a slider", async (question) => {
  const min = -9007199254740993n;
  const max = 9007199254740993n;
  const state = await renderSurvey(withConstraints(question, { min, max }));
  participate();
  expect(screen.queryByRole("slider")).not.toBeInTheDocument();
  expect(screen.queryByRole("textbox")).not.toBeInTheDocument();
  expect(state().valid).toBe(false);
  ([["Minimum", min], ["Increase", min + 1n], ["Maximum", max], ["Decrease", max - 1n]] as const).forEach(([name, value]) => {
    fireEvent.click(screen.getByRole("button", { name: `${name}: Extra` }));
    expect(state().valid).toBe(true);
    expect(state().response?.answers).toMatchObject({ answers: [
      { optionIndex: 0 },
      question.type === "rating" ? { ratings: [{ rating: value }] } : { value },
    ] });
    expect(screen.getByRole("button", { name: "Decrease: Extra" })).toHaveProperty("disabled", value === min);
    expect(screen.getByRole("button", { name: "Increase: Extra" })).toHaveProperty("disabled", value === max);
  });
  fireEvent.click(screen.getByRole("button", { name: "Clear answer: Extra" }));
  expect(state().valid).toBe(false);
});

it.each(numericQuestions)("requires explicit selection for a single $type value", async (question) => {
  const state = await renderSurvey(withConstraints(question, { min: 7n, max: 8n, step: 3n }));
  participate();
  expect(state().valid).toBe(false);
  expect(screen.queryByRole("slider")).not.toBeInTheDocument();
  fireEvent.click(screen.getByRole("button", { name: "Select 7: Extra" }));
  expect(state().valid).toBe(true);
  fireEvent.click(screen.getByRole("button", { name: "Clear answer: Extra" }));
  expect(state().valid).toBe(false);
});

it("keeps multiple numeric answers and rating options independent and resets on a survey change", async () => {
  const state = await renderSurvey([
    { type: "numericRange", prompt: "First number", constraints: { min: 10n, max: 20n } },
    { type: "numericRange", prompt: "Second number", constraints: { min: 100n, max: 200n, step: 5n } },
    { type: "rating", prompt: "Rate", options, scale: { type: "numeric", constraints: { min: -2n, max: 2n } }, requireAll: true, required: true },
  ]);
  participate();
  ["First number", "Second number", "Extra"].forEach((name) => {
    fireEvent.keyDown(screen.getByRole("slider", { name }), { key: "Home" });
  });
  expect(state().valid).toBe(false);
  fireEvent.keyDown(screen.getByRole("slider", { name: "Other" }), { key: "End" });
  expect(state().valid).toBe(true);
  fireEvent.click(screen.getByRole("button", { name: "Clear answer: First number" }));
  expect(state().response?.answers).toMatchObject({ answers: [
    { questionIndex: 0, optionIndex: 0 },
    { questionIndex: 2, value: 100n },
    {
      questionIndex: 3,
      ratings: [
        { optionIndex: 0, rating: -2n }, { optionIndex: 1, rating: 2n },
      ],
    },
  ] });
  fireEvent.click(screen.getByRole("button", { name: "Clear answer: Extra" }));
  expect(state().valid).toBe(false);
  act(() => state.reset());
  expect(state()).toMatchObject({ participating: false, valid: true, response: null });
  expect(screen.queryByRole("slider")).not.toBeInTheDocument();
});

it("preserves mixed question methods alongside the numeric control", async () => {
  const custom: Extract<Question, { type: "custom" }> = {
    type: "custom", prompt: "Custom", methodSchema: { uri: "https://example.com/schema", hash: new Uint8Array(32) },
  };
  const state = await renderSurvey([
    { type: "numericRange", prompt: "Number", constraints: { min: 7n, max: 35n, step: 7n } },
    { type: "multiSelect", prompt: "Select", options: { type: "options", labels: ["Selected", "Not selected"] }, minSelections: 0, maxSelections: 1 },
    { type: "ranking", prompt: "Rank", options: { type: "options", labels: ["Ranked first", "Ranked second"] }, minRanked: 1, maxRanked: 2 },
    { type: "rating", prompt: "Labels", options: { type: "options", labels: ["Label rating", "Another label rating"] }, scale: { type: "labels", labels: ["Low", "High"] }, requireAll: false },
    { type: "pointsAllocation", prompt: "Budget", options: { type: "options", labels: ["Activity A", "Activity B"] }, budget: 10 },
    custom,
  ], {
    [customMethodKey(custom)]: ({ onChange }) => <button type="button" onClick={() => onChange(42n)}>Custom answer</button>,
  });
  participate();
  fireEvent.keyDown(screen.getByRole("slider", { name: "Number" }), { key: "ArrowRight" });
  ["Selected", "Ranked second", "Ranked first"].forEach((name) => fireEvent.click(screen.getByRole("checkbox", { name })));
  fireEvent.mouseDown(screen.getByRole("combobox", { name: "Label rating" }));
  fireEvent.click(screen.getByRole("option", { name: "High" }));
  fireEvent.change(screen.getByRole("spinbutton", { name: "Activity A" }), { target: { value: "4" } });
  expect(state().valid).toBe(false);
  fireEvent.change(screen.getByRole("spinbutton", { name: "Activity B" }), { target: { value: "6" } });
  fireEvent.click(screen.getByRole("button", { name: "Custom answer" }));
  expect(state().valid).toBe(true);
  expect(state().response?.answers).toMatchObject({ answers: [
    { questionIndex: 0, optionIndex: 0 },
    { questionIndex: 1, value: 14n },
    { questionIndex: 2, optionIndices: [0] },
    { questionIndex: 3, ranking: [1, 0] },
    { questionIndex: 4, ratings: [{ optionIndex: 0, rating: 1n }] },
    {
      questionIndex: 5,
      allocations: [
        { optionIndex: 0, points: 4 }, { optionIndex: 1, points: 6 },
      ],
    },
    { questionIndex: 6, value: 42n },
  ] });
});

it("keeps unsupported custom methods optional alongside numeric answers", async () => {
  const state = await renderSurvey([
    { type: "numericRange", prompt: "Number", constraints: { min: 1n, max: 3n } },
    { type: "custom", prompt: "Custom", methodSchema: { uri: "https://example.com/schema", hash: new Uint8Array(32) } },
  ]);
  participate();
  fireEvent.keyDown(screen.getByRole("slider", { name: "Number" }), { key: "Home" });
  expect(screen.getByText("This custom survey method is not supported by GovTool.")).toBeInTheDocument();
  expect(state().valid).toBe(true);
  expect(state().response?.answers).toMatchObject({
    answers: [{ questionIndex: 0 }, { questionIndex: 1, value: 1n }],
  });
});

it.each(numericQuestions)("rejects malformed $type answers independently of the input control", (question) => {
  ["", " ", "1\n", "0x10", "0b10", "1e2", "1.5", "-", "+1", 1, undefined].forEach((value) => {
    expect(buildAnswer(question, 0, question.type === "rating" ? [value] : value, true)).toBeNull();
  });
});
