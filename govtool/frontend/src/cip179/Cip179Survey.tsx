import { type ComponentType, useEffect, useId, useMemo, useState } from "react";
import {
  Alert,
  Box,
  Button,
  Checkbox,
  FormControlLabel,
  FormLabel,
  MenuItem,
  Radio,
  RadioGroup,
  Select,
  Slider,
  TextField,
  Typography,
  type BoxProps,
} from "@mui/material";
import {
  Role,
  SPEC_VERSION,
  validateResponse,
  type AnswerItem,
  type Metadatum,
  type NumericConstraints,
  type OptionsOrCount,
  type Question,
  type SurveyDefinition,
  type SurveyResponse,
} from "cip-179";

import { API } from "@services";
import type { ProposalData } from "@models";
import {
  decodeDefinition,
  enrichDefinition,
  getRenderabilityProblem,
  parseSurveyLink,
  type SurveyDefinitionEnvelope,
  type SurveyLink,
} from "./core";

export type Cip179Participation = {
  participating: boolean;
  valid: boolean;
  response: SurveyResponse | null;
  definition: SurveyDefinition | null;
};

export type CustomQuestionRendererProps = {
  question: Extract<Question, { type: "custom" }>;
  value: Metadatum | undefined;
  onChange: (value: Metadatum) => void;
};

export type CustomQuestionRenderer = ComponentType<CustomQuestionRendererProps>;
export type CustomQuestionRenderers = Readonly<Record<string, CustomQuestionRenderer>>;

type Props = {
  dRepId?: string;
  proposal: ProposalData;
  onChange: (state: Cip179Participation) => void;
  customRenderers?: CustomQuestionRenderers;
};

const bytesToHex = (bytes: Uint8Array): string =>
  Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join("");

const surveyItemKey = (...indices: number[]): string => indices.join("-");

export const customMethodKey = (
  question: Extract<Question, { type: "custom" }>,
): string => `${question.methodSchema.uri}#${bytesToHex(question.methodSchema.hash)}`;

const optionLabels = (options: OptionsOrCount): readonly string[] =>
  (options.type === "options"
    ? options.labels
    : Array.from({ length: options.count }, (_, index) => `Option ${index + 1}`));

const parseDecimalInteger = (value: unknown): bigint | null =>
  (typeof value === "string" && /^-?[0-9]+$/.test(value)
    ? BigInt(value)
    : null);

const NumericAnswerField = ({
  constraints: { min, max, step = 1n },
  value,
  onChange,
  label,
  sx,
}: {
  constraints: NumericConstraints;
  value: string;
  onChange: (value: string) => void;
  label: string;
  sx?: BoxProps["sx"];
}) => {
  const descriptionId = useId();
  const parsed = parseDecimalInteger(value);
  const lastIndex = (max - min) / step;
  const index = parsed === null ? 0n : (parsed - min) / step;
  const selectIndex = (next: bigint) => {
    const bounded = next < 0n ? 0n : next > lastIndex ? lastIndex : next;
    onChange(String(min + bounded * step));
  };
  // Only slider indices use Number; answer values must retain full integer precision.
  const selectSliderIndex = (next: number | number[]) => {
    if (typeof next === "number" && Number.isSafeInteger(next)) selectIndex(BigInt(next));
  };
  return (
    <Box sx={sx}>
      <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between", minHeight: 36, gap: 1 }}>
        <Typography sx={{ fontWeight: 600, overflowWrap: "anywhere" }}>
          {parsed === null ? "Not answered" : String(parsed)}
        </Typography>
        {parsed !== null && (
          <Button size="small" aria-label={`Clear answer: ${label}`} onClick={() => onChange("")}>
            Clear answer
          </Button>
        )}
      </Box>
      {lastIndex === 0n ? (
        <Button aria-label={`Select ${min}: ${label}`} onClick={() => selectIndex(0n)}>
          Select {String(min)}
        </Button>
      ) : lastIndex <= BigInt(Number.MAX_SAFE_INTEGER) ? (
        <Box sx={{ px: 1 }}>
          <Slider
            aria-label={label}
            slotProps={{ input: { "aria-describedby": descriptionId } }}
            getAriaValueText={() => (parsed === null ? "Not answered" : String(parsed))}
            min={0}
            max={Number(lastIndex)}
            step={1}
            value={Number(index)}
            valueLabelDisplay="auto"
            valueLabelFormat={(position) => String(min + BigInt(position) * step)}
            onChange={(_, next) => selectSliderIndex(next)}
            // Selecting the initial thumb position must also record an explicit zero/minimum.
            onChangeCommitted={(_, next) => selectSliderIndex(next)}
            onKeyDownCapture={(event) => {
              const increment = event.shiftKey ? 10n : 1n;
              const next = {
                Home: 0n,
                End: lastIndex,
                ArrowLeft: index - increment,
                ArrowDown: index - increment,
                ArrowRight: index + increment,
                ArrowUp: index + increment,
                PageDown: index - 10n,
                PageUp: index + 10n,
              }[event.key];
              if (next === undefined) return;
              event.preventDefault();
              event.stopPropagation();
              selectIndex(next);
            }}
          />
        </Box>
      ) : (
        <Box role="group" aria-label={label} aria-describedby={descriptionId} sx={{ display: "flex", flexWrap: "wrap" }}>
          <Button aria-label={`Minimum: ${label}`} onClick={() => selectIndex(0n)}>Minimum</Button>
          <Button aria-label={`Decrease: ${label}`} disabled={parsed !== null && index === 0n} onClick={() => selectIndex(index - 1n)}>−</Button>
          <Button aria-label={`Increase: ${label}`} disabled={index === lastIndex} onClick={() => selectIndex(index + 1n)}>+</Button>
          <Button aria-label={`Maximum: ${label}`} onClick={() => selectIndex(lastIndex)}>Maximum</Button>
        </Box>
      )}
      <Typography id={descriptionId} variant="caption" color="text.secondary" sx={{ overflowWrap: "anywhere" }}>
        {`${min} to ${max}, step ${step}`}
      </Typography>
    </Box>
  );
};

export const buildAnswer = (
  question: Question,
  questionIndex: number,
  value: unknown,
  touched: boolean,
): AnswerItem | null => {
  if (!touched) return null;
  switch (question.type) {
    case "singleChoice":
      return typeof value === "number"
        ? { type: "singleChoice", questionIndex, optionIndex: value }
        : null;
    case "multiSelect":
      return Array.isArray(value)
        ? { type: "multiSelect", questionIndex, optionIndices: value as number[] }
        : null;
    case "ranking":
      return Array.isArray(value) && value.length
        ? { type: "ranking", questionIndex, ranking: value as number[] }
        : null;
    case "numericRange": {
      const parsed = parseDecimalInteger(value);
      return parsed === null ? null : { type: "numeric", questionIndex, value: parsed };
    }
    case "pointsAllocation":
      return Array.isArray(value) &&
        value.every(
          (points) =>
            typeof points === "number" &&
            Number.isSafeInteger(points) &&
            points >= 0,
        )
        ? {
            type: "pointsAllocation",
            questionIndex,
            allocations: (value as number[]).flatMap((points, optionIndex) =>
              (points > 0 ? [{ optionIndex, points }] : []),
            ),
          }
        : null;
    case "rating": {
      if (!Array.isArray(value)) return null;
      const parsed = value.map(parseDecimalInteger);
      if (value.some((rating, index) => rating !== null && parsed[index] === null)) return null;
      const ratings = parsed.flatMap((rating, optionIndex) =>
        (rating === null ? [] : [{ optionIndex, rating }]));
      return ratings.length ? { type: "rating", questionIndex, ratings } : null;
    }
    case "custom":
      return value === undefined
        ? null
        : { type: "custom", questionIndex, value: value as Metadatum };
    default:
      return null;
  }
};

const ratingValues = (question: Extract<Question, { type: "rating" }>) => {
  if (question.scale.type === "labels") {
    return question.scale.labels.map((label, index) => ({
      label,
      value: BigInt(index),
    }));
  }
  if (question.scale.type === "count") {
    return Array.from({ length: question.scale.count }, (_, index) => ({
      label: String(index + 1),
      value: BigInt(index),
    }));
  }
  return [];
};

export const Cip179Survey = ({
  customRenderers = {},
  dRepId,
  proposal,
  onChange,
}: Props) => {
  const [link, setLink] = useState<SurveyLink | null>(null);
  const [definition, setDefinition] = useState<SurveyDefinition | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [warning, setWarning] = useState<string | null>(null);
  const [participating, setParticipating] = useState(false);
  const [values, setValues] = useState<Record<number, unknown>>({});
  const [touched, setTouched] = useState<Set<number>>(new Set());

  useEffect(() => {
    const nextLink = parseSurveyLink(proposal.json);
    setLink(nextLink);
    setDefinition(null);
    setError(null);
    setWarning(null);
    setParticipating(false);
    setValues({});
    setTouched(new Set());
    if (!nextLink) return;
    let active = true;
    API.get<SurveyDefinitionEnvelope>(
      `/survey/definition/${nextLink.txId}/${nextLink.index}`,
    )
      .then(async ({ data }) => {
        const decoded = decodeDefinition(
          data,
          nextLink,
          proposal.expiryEpochNo,
        );
        const renderabilityProblem = getRenderabilityProblem(decoded);
        if (renderabilityProblem) throw new Error(renderabilityProblem);
        if (active) setDefinition(decoded);
        try {
          const loaded = await enrichDefinition(decoded);
          if (active) setDefinition(loaded);
        } catch (reason) {
          if (active) {
            setWarning(
              reason instanceof Error
                ? reason.message
                : "External survey presentation is unavailable",
            );
          }
        }
      })
      .catch((reason: unknown) => {
        if (active) {
          setError(reason instanceof Error ? reason.message : "Survey unavailable");
        }
      });
    return () => {
      active = false;
    };
  }, [proposal.expiryEpochNo, proposal.json]);

  const responseState = useMemo<Cip179Participation>(() => {
    if (!participating || !definition || !link || !dRepId) {
      return {
        participating,
        valid: !participating,
        response: null,
        definition,
      };
    }
    if (!/^[0-9a-fA-F]{56}$/.test(dRepId)) {
      return { participating, valid: false, response: null, definition };
    }
    const builtAnswers = definition.questions.map((question, index) =>
      buildAnswer(question, index, values[index], touched.has(index)),
    );
    const answers = builtAnswers.flatMap((answer) => (answer ? [answer] : []));
    const hasInvalidTouchedAnswer = builtAnswers.some(
      (answer, index) => touched.has(index) && answer === null,
    );
    const response: SurveyResponse = {
      specVersion: SPEC_VERSION,
      surveyRef: {
        txId: Uint8Array.from(
          { length: 32 },
          (_, index) => parseInt(link.txId.slice(index * 2, index * 2 + 2), 16),
        ),
        index: link.index,
      },
      role: Role.DRep,
      credential: {
        type: "key",
        keyHash: Uint8Array.from(
          { length: 28 },
          (_, index) => parseInt(dRepId.slice(index * 2, index * 2 + 2), 16),
        ),
      },
      answers: { type: "public", answers },
    };
    const validationDefinition: SurveyDefinition =
      definition.submissionMode.type === "sealed"
        ? { ...definition, submissionMode: { type: "public" } }
        : definition;
    const valid =
      answers.length > 0 &&
      !hasInvalidTouchedAnswer &&
      validateResponse(validationDefinition, response).length === 0;
    return { participating, valid, response: valid ? response : null, definition };
  }, [dRepId, definition, link, participating, touched, values]);

  useEffect(() => onChange(responseState), [onChange, responseState]);

  if (!link) return null;
  if (error) {
    return (
      <Alert severity="warning">
        Linked survey response unavailable: {error}
      </Alert>
    );
  }
  if (!definition) return <Typography>Loading linked survey...</Typography>;

  const eligible = definition.eligibleRoles.includes(Role.DRep);
  return (
    <Box sx={{ borderTop: 1, borderColor: "divider", mt: 4, pt: 3, width: "100%" }}>
      <Typography variant="h6">{definition.title || "Linked survey"}</Typography>
      {definition.description && <Typography sx={{ mt: 1 }}>{definition.description}</Typography>}
      {warning && (
        <Alert severity="warning" sx={{ mt: 2 }}>
          External presentation unavailable: {warning}. Index-based labels are shown.
        </Alert>
      )}
      {!eligible ? (
        <Alert severity="info" sx={{ mt: 2 }}>
          This survey does not accept DRep responses.
        </Alert>
      ) : (
        <FormControlLabel
          sx={{ mt: 2 }}
          control={
            <Checkbox
              checked={participating}
              onChange={(event) => setParticipating(event.target.checked)}
            />
          }
          label="Include a survey response with this governance vote"
        />
      )}
      {eligible && participating && (
        <Box sx={{ display: "flex", flexDirection: "column", gap: 3, mt: 2 }}>
          {definition.questions.map((question, index) => {
            const labels = "options" in question ? optionLabels(question.options) : [];
            const markTouched = (value: unknown) => {
              setValues((current) => ({ ...current, [index]: value }));
              setTouched((current) => {
                const next = new Set(current);
                const empty =
                  (question.type === "numericRange" && value === "") ||
                  (question.type === "ranking" && Array.isArray(value) && value.length === 0) ||
                  (question.type === "rating" && Array.isArray(value) && value.every((rating) => rating === null));
                if (empty) next.delete(index);
                else next.add(index);
                return next;
              });
            };
            return (
              <Box
                component="fieldset"
                key={surveyItemKey(index)}
                sx={{
                  border: 0,
                  display: "flex",
                  flexDirection: "column",
                  m: 0,
                  minWidth: 0,
                  p: 0,
                  width: "100%",
                }}
              >
                <FormLabel component="legend" required={question.required}>
                  {question.prompt}
                </FormLabel>
                {question.type === "singleChoice" && (
                  <RadioGroup
                    value={values[index] ?? ""}
                    onChange={(event) => markTouched(Number(event.target.value))}
                  >
                    {labels.map((label, optionIndex) => (
                      <FormControlLabel
                        key={surveyItemKey(index, optionIndex)}
                        value={optionIndex}
                        control={<Radio />}
                        label={label}
                      />
                    ))}
                  </RadioGroup>
                )}
                {(question.type === "multiSelect" || question.type === "ranking") &&
                  labels.map((label, optionIndex) => {
                    const selected = (values[index] as number[] | undefined) ?? [];
                    const position = selected.indexOf(optionIndex);
                    return (
                      <FormControlLabel
                        key={surveyItemKey(index, optionIndex)}
                        control={
                          <Checkbox
                            checked={position >= 0}
                            onChange={(event) =>
                              markTouched(
                                event.target.checked
                                  ? [...selected, optionIndex]
                                  : selected.filter((item) => item !== optionIndex),
                              )
                            }
                          />
                        }
                        label={
                          question.type === "ranking" && position >= 0
                            ? `${position + 1}. ${label}`
                            : label
                        }
                      />
                    );
                  })}
                {question.type === "numericRange" && (
                  <NumericAnswerField
                    constraints={question.constraints}
                    value={(values[index] as string | undefined) ?? ""}
                    onChange={markTouched}
                    label={question.prompt || `Question ${index + 1}`}
                  />
                )}
                {question.type === "pointsAllocation" &&
                  labels.map((label, optionIndex) => {
                    const points =
                      (values[index] as number[] | undefined) ?? labels.map(() => 0);
                    return (
                      <TextField
                        key={surveyItemKey(index, optionIndex)}
                        label={label}
                        value={points[optionIndex] ?? 0}
                        onChange={(event) => {
                          const next = [...points];
                          next[optionIndex] = Math.max(0, Number(event.target.value));
                          markTouched(next);
                        }}
                        inputProps={{ min: 0, max: question.budget, step: 1 }}
                        type="number"
                        sx={{ mt: 1 }}
                      />
                    );
                  })}
                {question.type === "rating" &&
                  labels.map((label, optionIndex) => {
                    const ratings =
                      (values[index] as Array<string | null> | undefined) ??
                      labels.map(() => null);
                    return (
                      <Box
                        key={surveyItemKey(index, optionIndex)}
                        sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 2, mt: 1 }}
                      >
                        <Typography sx={{ flex: 1 }}>{label}</Typography>
                        {question.scale.type === "numeric" ? (
                          <NumericAnswerField
                            constraints={question.scale.constraints}
                            value={ratings[optionIndex] ?? ""}
                            onChange={(value) => {
                              const next = [...ratings];
                              next[optionIndex] = value || null;
                              markTouched(next);
                            }}
                            label={label}
                            sx={{ minWidth: 180, flex: 1 }}
                          />
                        ) : (
                          <Select
                            displayEmpty
                            inputProps={{ "aria-label": label }}
                            value={ratings[optionIndex] ?? ""}
                            onChange={(event) => {
                              const next = [...ratings];
                              next[optionIndex] = event.target.value || null;
                              markTouched(next);
                            }}
                            sx={{ minWidth: 140 }}
                          >
                            <MenuItem value="">Not rated</MenuItem>
                            {ratingValues(question).map((rating) => (
                              <MenuItem
                                key={rating.value.toString()}
                                value={rating.value.toString()}
                              >
                                {rating.label}
                              </MenuItem>
                            ))}
                          </Select>
                        )}
                      </Box>
                    );
                  })}
                {question.type === "custom" && (
                  (() => {
                    const Renderer = customRenderers[customMethodKey(question)];
                    return Renderer ? (
                      <Renderer
                        question={question}
                        value={values[index] as Metadatum | undefined}
                        onChange={markTouched}
                      />
                    ) : (
                      <Alert severity="info" sx={{ mt: 1 }}>
                        This custom survey method is not supported by GovTool.
                      </Alert>
                    );
                  })()
                )}
              </Box>
            );
          })}
          {!responseState.valid && (
            <Alert severity="info">
              Complete the selected survey response before submitting.
            </Alert>
          )}
        </Box>
      )}
    </Box>
  );
};
