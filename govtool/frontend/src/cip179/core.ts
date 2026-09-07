import { blake2bHex } from "blakejs";
import {
  decodePayload,
  validateDefinition,
  type ContentAnchor,
  type OptionsOrCount,
  type Question,
  type RatingScale,
  type SurveyDefinition,
} from "cip-179";
import { parseCip179Link } from "cip-179/domain";

import { metadatumCodec } from "./csl";

export type SurveyLink = { txId: string; index: number };
export type SurveyDefinitionEnvelope = {
  txId: string;
  surveyIndex: number;
  metadataLabel: 17;
  payloadCborHex: string;
};

export const MAX_RENDERED_SURVEY_ITEMS = 100;

export const getRenderabilityProblem = (
  definition: SurveyDefinition,
): string | null => {
  if (definition.questions.length > MAX_RENDERED_SURVEY_ITEMS) {
    return `Survey has more than ${MAX_RENDERED_SURVEY_ITEMS} questions`;
  }
  const optionsIndex = definition.questions.findIndex(
    (question) =>
      "options" in question &&
      (question.options.type === "options"
        ? question.options.labels.length
        : question.options.count) > MAX_RENDERED_SURVEY_ITEMS,
  );
  if (optionsIndex >= 0) {
    return `Question ${optionsIndex + 1} has more than ${MAX_RENDERED_SURVEY_ITEMS} options`;
  }
  const ratingIndex = definition.questions.findIndex(
    (question) =>
      question.type === "rating" &&
      question.scale.type !== "numeric" &&
      (question.scale.type === "labels"
        ? question.scale.labels.length
        : question.scale.count) > MAX_RENDERED_SURVEY_ITEMS,
  );
  if (ratingIndex >= 0) {
    return `Question ${ratingIndex + 1} has more than ${MAX_RENDERED_SURVEY_ITEMS} rating levels`;
  }
  return null;
};

export const hexToBytes = (hex: string): Uint8Array => {
  if (hex.length % 2 !== 0 || !/^[0-9a-fA-F]*$/.test(hex)) {
    throw new Error("Invalid hex value");
  }
  return Uint8Array.from(
    { length: hex.length / 2 },
    (_, index) => parseInt(hex.slice(index * 2, index * 2 + 2), 16),
  );
};

export const parseSurveyLink = (metadata: unknown): SurveyLink | null => {
  if (!metadata || typeof metadata !== "object") return null;
  const { body } = metadata as Record<string, unknown>;
  if (!body || typeof body !== "object") return null;
  const link = (body as Record<string, unknown>).cip179;
  if (
    !link ||
    typeof link !== "object" ||
    (link as Record<string, unknown>).specVersion !== 5
  ) {
    return null;
  }
  const { surveyRef } = parseCip179Link(metadata);
  return surveyRef;
};

export const decodeDefinition = (
  envelope: SurveyDefinitionEnvelope,
  expected: SurveyLink,
  expiryEpoch: number | undefined,
): SurveyDefinition => {
  if (
    envelope.txId.toLowerCase() !== expected.txId.toLowerCase() ||
    envelope.surveyIndex !== expected.index ||
    envelope.metadataLabel !== 17
  ) {
    throw new Error("Survey definition response does not match the requested reference");
  }
  const decoded = metadatumCodec.cborToMetadatum(
    hexToBytes(envelope.payloadCborHex),
  );
  // db-sync stores each tx_metadata.bytes row as a singleton metadata map.
  // Keep accepting an inner payload for hosts that already extract the label.
  const labelPayload = decoded instanceof Map ? decoded.get(17n) : decoded;
  if (labelPayload === undefined) {
    throw new Error("Survey metadata does not contain label 17");
  }
  const payload = decodePayload(labelPayload);
  if (payload.type !== "definitions") {
    throw new Error("Label 17 payload is not a survey definition");
  }
  const definition = payload.definitions[envelope.surveyIndex];
  if (!definition) throw new Error("Survey index does not exist");
  const problems = validateDefinition(definition);
  if (problems.length) throw new Error(problems.join("; "));
  if (expiryEpoch !== undefined && definition.endEpoch !== expiryEpoch - 1) {
    throw new Error("Survey end epoch does not match the governance action");
  }
  return definition;
};

const fillOptions = (
  source: OptionsOrCount,
  labels: string[] | undefined,
): OptionsOrCount =>
  (source.type === "count" && labels?.length === source.count
    ? { type: "options", labels }
    : source);

const fillScale = (
  source: RatingScale,
  labels: string[] | undefined,
): RatingScale =>
  (source.type === "count" && labels?.length === source.count
    ? { type: "labels", labels }
    : source);

type PresentationQuestion = {
  prompt?: string;
  options?: string[];
  ratingLabels?: string[];
};

const fetchContentAnchor = async (anchor: ContentAnchor): Promise<unknown> => {
  const uri = anchor.uri.startsWith("ipfs://")
    ? `https://ipfs.io/ipfs/${anchor.uri.slice(7)}`
    : anchor.uri;
  if (!uri.startsWith("https://")) throw new Error("Unsupported anchor URI");
  const controller = new AbortController();
  const timeout = globalThis.setTimeout(() => controller.abort(), 10_000);
  const response = await fetch(uri, { signal: controller.signal }).finally(() =>
    globalThis.clearTimeout(timeout),
  );
  if (!response.ok) throw new Error("External survey content is unavailable");
  const bytes = new Uint8Array(await response.arrayBuffer());
  if (
    blake2bHex(bytes, undefined, 32) !==
    Array.from(anchor.hash, (byte) => byte.toString(16).padStart(2, "0")).join("")
  ) {
    throw new Error("External survey content hash does not match");
  }
  return JSON.parse(new TextDecoder().decode(bytes));
};

const applyQuestion = (
  question: Question,
  presentation: PresentationQuestion | undefined,
): Question => {
  const prompt = question.prompt || presentation?.prompt || "";
  if (question.type === "custom" || question.type === "numericRange") {
    return { ...question, prompt };
  }
  if (question.type === "rating") {
    return {
      ...question,
      prompt,
      options: fillOptions(question.options, presentation?.options),
      scale: fillScale(question.scale, presentation?.ratingLabels),
    };
  }
  return {
    ...question,
    prompt,
    options: fillOptions(question.options, presentation?.options),
  };
};

export const enrichDefinition = async (
  definition: SurveyDefinition,
): Promise<SurveyDefinition> => {
  let enriched = definition;
  if (definition.contentAnchor) {
    const raw = await fetchContentAnchor(definition.contentAnchor);
    if (!raw || typeof raw !== "object") throw new Error("Invalid presentation");
    const presentation = raw as Record<string, unknown>;
    if (
      presentation.specVersion !== 5 ||
      presentation.kind !== "cardano-survey-presentation"
    ) {
      throw new Error("Invalid presentation kind");
    }
    const questions = Array.isArray(presentation.questions)
      ? (presentation.questions as PresentationQuestion[])
      : [];
    enriched = {
      ...definition,
      title: definition.title || String(presentation.title || ""),
      description:
        definition.description || String(presentation.description || ""),
      questions: definition.questions.map((question, index) =>
        applyQuestion(question, questions[index]),
      ),
    };
  }
  return enriched;
};
