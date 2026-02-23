import { useState, useEffect, useMemo, Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";
import { Trans } from "react-i18next";

import { Button, Radio, Typography } from "@atoms";
import { useModal } from "@context";
import {
  SurveyResponsePayload,
  useScreenDimension,
  useVoteActionForm,
  useTranslation,
  useGetVoterInfo,
  useGetVoteContextTextFromFile,
  useGetProposalSurveyQuery,
} from "@hooks";
import { formatDisplayDate, getFullGovActionId } from "@utils";
import { errorRed, fadedPurple } from "@/consts";
import { ProposalData, ProposalVote, Vote } from "@/models";
import { VoteContextModalState, SubmittedVotesModalState } from "../organisms";

type VoteActionFormProps = {
  setIsVoteSubmitted: Dispatch<SetStateAction<boolean>>;
  isInProgress?: boolean;
  previousVote?: ProposalVote | null;
  proposal: ProposalData;
};

export const VoteActionForm = ({
  setIsVoteSubmitted,
  previousVote,
  isInProgress,
  proposal,
  proposal: { expiryDate, expiryEpochNo },
}: VoteActionFormProps) => {
  const [surveyAnswers, setSurveyAnswers] = useState<
    Record<
      string,
      {
        selection?: number[];
        numericValue?: number;
        customValue?: string;
      }
    >
  >({});
  const [surveyError, setSurveyError] = useState<string | null>(null);
  const [voteContextHash, setVoteContextHash] = useState<string | undefined>();
  const [voteContextUrl, setVoteContextUrl] = useState<string | undefined>();
  const [showWholeVoteContext, setShowWholeVoteContext] =
    useState<boolean>(false);

  const { voter } = useGetVoterInfo();
  const fullProposalId = getFullGovActionId(proposal.txHash, proposal.index);
  const { data: proposalSurvey } = useGetProposalSurveyQuery(
    fullProposalId,
    proposal.type === "InfoAction",
  );
  const { voteContextText, valid: voteContextValid = true } =
    useGetVoteContextTextFromFile(voteContextUrl, voteContextHash) || {};

  const finalVoteContextText =
    previousVote && !voteContextUrl && !voteContextHash
      ? ""
      : voteContextText;

  const { isMobile } = useScreenDimension();
  const { openModal, closeModal } = useModal();
  const { t } = useTranslation();

  const {
    areFormErrors,
    confirmVote,
    isVoteLoading,
    registerInput,
    setValue,
    vote,
    canVote,
  } = useVoteActionForm({
    previousVote,
    voteContextHash,
    voteContextUrl,
    closeModal,
  });

  const handleVoteClick = (isVoteChanged: boolean) => {
    const shouldAttachSurveyResponse =
      proposalSurvey?.linked &&
      proposalSurvey?.linkValidation?.valid &&
      proposalSurvey?.surveyDetailsValidation?.valid &&
      proposalSurvey?.surveyRef &&
      proposalSurvey?.surveyDetails;

    let surveyResponsePayload: SurveyResponsePayload | undefined;
    if (shouldAttachSurveyResponse) {
      let hasInvalidSurveyAnswer = false;
      const answers = proposalSurvey.surveyDetails.questions.flatMap((question) => {
        const answer = surveyAnswers[question.questionId];
        if (!answer) return [];

        if (Array.isArray(answer.selection)) {
          return [
            {
              questionId: question.questionId,
              selection: answer.selection,
            },
          ];
        }

        if (
          typeof answer.numericValue === "number" &&
          Number.isFinite(answer.numericValue)
        ) {
          return [
            {
              questionId: question.questionId,
              numericValue: answer.numericValue,
            },
          ];
        }

        if (typeof answer.customValue === "string" && answer.customValue.trim()) {
          try {
            const customValue = JSON.parse(answer.customValue);
            return [
              {
                questionId: question.questionId,
                customValue,
              },
            ];
          } catch (_error) {
            setSurveyError("Invalid custom survey answer JSON.");
            hasInvalidSurveyAnswer = true;
            return [];
          }
        }

        return [];
      });

      if (hasInvalidSurveyAnswer) {
        return;
      }

      if (answers.length) {
        surveyResponsePayload = {
          specVersion: "1.0.0",
          surveyTxId: proposalSurvey.surveyRef.surveyTxId,
          surveyHash: proposalSurvey.surveyRef.surveyHash,
          answers,
        };
      }
    }

    setSurveyError(null);
    openModal({
      type: "voteContext",
      state: {
        onSubmit: (url, hash) => {
          setVoteContextUrl(url);
          setVoteContextHash(hash ?? undefined);
          confirmVote(vote as Vote, url, hash, surveyResponsePayload);
          setVoteContextData(url, hash);
        },
        vote: vote as Vote,
        confirmVote,
        previousRationale: isVoteChanged ? undefined : finalVoteContextText,
      } satisfies VoteContextModalState,
    });
  };

  const setVoteContextData = (url: string, hash: string | null) => {
    setVoteContextUrl(url);
    setVoteContextHash(hash ?? undefined);
  };

  useEffect(() => {
    if (previousVote?.vote) {
      setValue("vote", previousVote.vote);
      setIsVoteSubmitted(true);
    }
  }, [previousVote?.vote, setValue, setIsVoteSubmitted]);

  useEffect(() => {
    if (previousVote?.url) {
      setVoteContextUrl(previousVote.url);
    }
    if (previousVote?.metadataHash) {
      setVoteContextHash(previousVote.metadataHash);
    }
  }, [previousVote?.metadataHash, previousVote?.url]);

  const renderCancelButton = useMemo(
    () => (
      <Button
        data-testid="cancel-button"
        onClick={() => setValue("vote", previousVote?.vote ?? "")}
        variant="outlined"
        size="extraLarge"
        sx={{
          width: "100%",
        }}
      >
        {t("cancel")}
      </Button>
    ),
    [previousVote?.vote, setValue],
  );

  const renderChangeVoteButton = useMemo(
    () => (
      <Button
        data-testid="change-vote"
        onClick={() => handleVoteClick(true)}
        disabled={!canVote}
        isLoading={isVoteLoading}
        variant="contained"
        sx={{
          borderRadius: 50,
          textTransform: "none",
          width: "100%",
          height: 48,
        }}
      >
        {t("govActions.changeVote")}
      </Button>
    ),
    [canVote, handleVoteClick, isVoteLoading, t],
  );

  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        width: "full",
        ...(isInProgress && { opacity: 0.5 }),
      }}
    >
      <Box flex={1} display="flex" flexDirection="column" alignItems="center">
        {previousVote?.date ? (
          <>
            <Typography
              variant="body1"
              sx={{
                whiteSpace: "pre-line",
                fontWeight: 400,
                mb: 1,
                alignSelf: "start",
              }}
            >
              <Trans
                i18nKey="govActions.castVote"
                values={{
                  vote: previousVote?.vote.toLocaleUpperCase(),
                  date: formatDisplayDate(previousVote.date),
                  epoch: previousVote.epochNo,
                }}
                components={[<span style={{ fontWeight: 600 }} key="0" />]}
              />
            </Typography>
            <Typography
              variant="caption"
              sx={{ lineHeight: "18px", alignSelf: "start" }}
            >
              {t("govActions.castVoteDeadline", {
                date: formatDisplayDate(
                  expiryDate ?? "",
                  "yyyy-MM-dd HH:mm:ss",
                ),
                epoch: expiryEpochNo,
              })}
            </Typography>
          </>
        ) : (
          <Typography variant="body1">
            {t("govActions.chooseHowToVote")}
          </Typography>
        )}
        <Box
          mt={3}
          sx={{
            alignSelf: "stretch",
            display: "flex",
            flexDirection: "column",
            gap: 2,
          }}
        >
          <Radio
            dataTestId="yes-radio"
            isChecked={vote?.toLowerCase() === "yes"}
            name="vote"
            register={registerInput}
            setValue={setValue}
            title={t("votes.yes")}
            value="yes"
            disabled={isInProgress}
          />
          <Radio
            dataTestId="no-radio"
            isChecked={vote?.toLowerCase() === "no"}
            name="vote"
            register={registerInput}
            setValue={setValue}
            title={t("votes.no")}
            value="no"
            disabled={isInProgress}
          />
          <Radio
            dataTestId="abstain-radio"
            isChecked={vote?.toLowerCase() === "abstain"}
            name="vote"
            register={registerInput}
            setValue={setValue}
            title={t("votes.abstain")}
            value="abstain"
            disabled={isInProgress}
          />
        </Box>
        {proposalSurvey?.linked &&
          proposalSurvey?.linkValidation?.valid &&
          proposalSurvey?.surveyDetailsValidation?.valid &&
          proposalSurvey?.surveyDetails && (
            <Box
              sx={{
                mt: 3,
                width: "100%",
                border: "1px solid rgba(47, 98, 220, 0.20)",
                borderRadius: "12px",
                p: 2,
              }}
            >
              <Typography variant="body1" sx={{ fontWeight: 600 }}>
                Survey response
              </Typography>
              <Typography variant="caption" sx={{ display: "block", mt: 0.5 }}>
                {proposalSurvey.surveyDetails.title}
              </Typography>
              {proposalSurvey.surveyDetails.questions.map((question) => {
                const localAnswer = surveyAnswers[question.questionId];
                const methodType = question.methodType;
                const selection = localAnswer?.selection ?? [];

                return (
                  <Box key={question.questionId} sx={{ mt: 2 }}>
                    <Typography variant="body2">{question.question}</Typography>
                    {(methodType ===
                      "urn:cardano:poll-method:single-choice:v1" ||
                      methodType === "urn:cardano:poll-method:multi-select:v1") &&
                      (question.options ?? []).map((option, optionIndex) => {
                        const isSingle =
                          methodType ===
                          "urn:cardano:poll-method:single-choice:v1";
                        const checked = selection.includes(optionIndex);

                        return (
                          <label
                            key={`${question.questionId}-${option}`}
                            style={{
                              display: "flex",
                              alignItems: "center",
                              gap: "8px",
                              marginTop: "8px",
                              cursor: "pointer",
                            }}
                          >
                            <input
                              type={isSingle ? "radio" : "checkbox"}
                              checked={checked}
                              onChange={() => {
                                setSurveyError(null);
                                if (isSingle) {
                                  setSurveyAnswers((prev) => ({
                                    ...prev,
                                    [question.questionId]: {
                                      selection: [optionIndex],
                                    },
                                  }));
                                  return;
                                }

                                setSurveyAnswers((prev) => {
                                  const currentSelection =
                                    prev[question.questionId]?.selection ?? [];
                                  const nextSelection = currentSelection.includes(
                                    optionIndex,
                                  )
                                    ? currentSelection.filter(
                                        (item) => item !== optionIndex,
                                      )
                                    : [...currentSelection, optionIndex];
                                  return {
                                    ...prev,
                                    [question.questionId]: {
                                      selection: nextSelection,
                                    },
                                  };
                                });
                              }}
                            />
                            <span>{option}</span>
                          </label>
                        );
                      })}
                    {methodType === "urn:cardano:poll-method:numeric-range:v1" && (
                      <input
                        style={{
                          width: "100%",
                          marginTop: "8px",
                          padding: "8px",
                          border: "1px solid #D0D7E8",
                          borderRadius: "8px",
                        }}
                        type="number"
                        min={question.numericConstraints?.minValue}
                        max={question.numericConstraints?.maxValue}
                        step={question.numericConstraints?.step ?? 1}
                        value={localAnswer?.numericValue ?? ""}
                        onChange={(event) => {
                          setSurveyError(null);
                          const numericValue = Number(event.target.value);
                          setSurveyAnswers((prev) => ({
                            ...prev,
                            [question.questionId]: { numericValue },
                          }));
                        }}
                      />
                    )}
                    {![
                      "urn:cardano:poll-method:single-choice:v1",
                      "urn:cardano:poll-method:multi-select:v1",
                      "urn:cardano:poll-method:numeric-range:v1",
                    ].includes(methodType) && (
                      <textarea
                        style={{
                          width: "100%",
                          marginTop: "8px",
                          minHeight: "76px",
                          padding: "8px",
                          border: "1px solid #D0D7E8",
                          borderRadius: "8px",
                        }}
                        placeholder='Custom value JSON, e.g. {"rank":[0,1,2]}'
                        value={localAnswer?.customValue ?? ""}
                        onChange={(event) => {
                          setSurveyError(null);
                          setSurveyAnswers((prev) => ({
                            ...prev,
                            [question.questionId]: {
                              customValue: event.target.value,
                            },
                          }));
                        }}
                      />
                    )}
                  </Box>
                );
              })}
            </Box>
          )}
        {proposalSurvey?.linked &&
          (!proposalSurvey?.linkValidation?.valid ||
            !proposalSurvey?.surveyDetailsValidation?.valid) && (
            <Box sx={{ mt: 2, width: "100%" }}>
              <Typography variant="caption" sx={{ display: "block" }}>
                Linked survey is invalid:
              </Typography>
              {[
                ...(proposalSurvey?.linkValidation?.errors ?? []),
                ...(proposalSurvey?.surveyDetailsValidation?.errors ?? []),
              ].map((validationError) => (
                <Typography
                  key={validationError}
                  variant="caption"
                  sx={{ display: "block", mt: 0.5 }}
                >
                  • {validationError}
                </Typography>
              ))}
            </Box>
          )}
        {surveyError && (
          <Typography sx={{ fontSize: "14px", fontWeight: 700, color: errorRed.c500 }}>
            {surveyError}
          </Typography>
        )}
        {(voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter) && (
          <Button
            data-testid="show-votes-button"
            variant="text"
            size="large"
            disabled={isInProgress}
            sx={{
              mt: "26px",
              fontSize: "14px",
              fontWeight: "500",
              lineHeight: "20px",
            }}
            onClick={() => {
              openModal({
                type: "submittedVotes",
                state: {
                  ...proposal,
                  vote: previousVote?.vote,
                } satisfies SubmittedVotesModalState,
              });
            }}
          >
            {t("govActions.showVotes")}
          </Button>
        )}
        {
          !voteContextValid &&
            <Typography sx={{ fontSize: "14px", fontWeight: 700, color: errorRed.c500 }}>
                {t("govActions.invalidVoteContext")}
            </Typography>
        }
        {finalVoteContextText && (
          <>
            <Typography sx={{ fontSize: "14px", fontWeight: 500 }}>{t("govActions.yourVoteRationale")}</Typography>
            <Box
              sx={{
              display: "flex",
              flexDirection: "column",
              justifyContent: "space-between",
              width: "100%",
              mt: 2,
            }}
            >
              {finalVoteContextText && (
              <Box
                sx={{
              position: "relative",
              width: "100%",
              mt: 2,
              border: !showWholeVoteContext ? "1px solid #E1E1E1" : "none",
              borderRadius: "4px",
              backgroundColor: !showWholeVoteContext ? fadedPurple.c50 : "transparent",
              padding: 2
            }}
              >
                <Typography
                  variant="body2"
                  sx={{
                fontWeight: 400,
                color: "neutralGray",
                whiteSpace: "pre-wrap",
                ...(!showWholeVoteContext && {
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  display: "-webkit-box",
                  WebkitBoxOrient: "vertical",
                  WebkitLineClamp: 2,
                }),
              }}
                  data-testid="vote-rationale-context"
                >
                  {finalVoteContextText}
                </Typography>

                {!showWholeVoteContext && (
                <Box
                  sx={{
            display: "flex",
            justifyContent: "flex-end",
            position: "absolute",
            bottom: 8,
            right: 16,
            background: fadedPurple.c50,
          }}
                >
                  <Button
                    onClick={() => setShowWholeVoteContext(true)}
                    sx={{
              p: 0,
              minWidth: "unset",
              ":hover": { backgroundColor: "transparent" },
            }}
                    disableRipple
                    variant="text"
                    data-testid="show-more-button"
                  >
                    <Typography
                      variant="body2"
                      sx={{
                fontWeight: 400,
                color: "primaryBlue",
                borderBottom: "1px solid",
              }}
                    >
                      {t("showMore")}
                    </Typography>
                  </Button>
                </Box>
      )}
              </Box>
  )}

            </Box>
          </>
        )}

        <Box sx={{ mt: 4 }} />
      </Box>
      {previousVote?.vote && previousVote?.vote !== vote ? (
        <Box
          display="flex"
          flexDirection={isMobile ? "column" : "row"}
          justifyContent="space-between"
        >
          {isMobile ? renderChangeVoteButton : renderCancelButton}
          <Box px={1} py={isMobile ? 1.5 : 0} />
          {isMobile ? renderCancelButton : renderChangeVoteButton}
        </Box>
      ) : (
        // this button appears on gov action detail page to change vote or rationale.
        <Button
          data-testid="vote-button"
          variant="contained"
          disabled={
            (previousVote?.vote && previousVote?.vote === vote)
              ? false
              : !vote || areFormErrors
          }
          isLoading={isVoteLoading}
          onClick={() => handleVoteClick(false)}
          size="extraLarge"
        >
          {previousVote?.vote && previousVote?.vote === vote
            ? t("govActions.changeRationale")
            : t("govActions.vote")}
        </Button>
      )}
    </Box>
  );
};
