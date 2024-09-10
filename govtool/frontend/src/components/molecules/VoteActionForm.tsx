import { useState, useEffect, useMemo, Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";
import { Trans } from "react-i18next";

import { Button, Radio, Typography } from "@atoms";
import { orange } from "@consts";
import { useModal } from "@context";
import {
  useScreenDimension,
  useVoteActionForm,
  useTranslation,
  useGetVoterInfo,
  useGetVoteContextTextFromFile,
} from "@hooks";
import { formatDisplayDate } from "@utils";
import { ProposalVote } from "@/models";
import { VoteContextModalState, VotingPowerModalState } from "../organisms";

type VoteActionFormProps = {
  setIsVoteSubmitted: Dispatch<SetStateAction<boolean>>;
  expiryDate: string | undefined;
  expiryEpochNo: number | undefined;
  isInProgress?: boolean;
  previousVote?: ProposalVote;
  dRepYesVotes: number;
  dRepNoVotes: number;
  dRepAbstainVotes: number;
};

export const VoteActionForm = ({
  setIsVoteSubmitted,
  expiryDate,
  expiryEpochNo,
  previousVote,
  dRepAbstainVotes,
  dRepNoVotes,
  dRepYesVotes,
  isInProgress,
}: VoteActionFormProps) => {
  const [voteContextHash, setVoteContextHash] = useState<string | undefined>();
  const [voteContextUrl, setVoteContextUrl] = useState<string | undefined>();
  const [showWholeVoteContext, setShowWholeVoteContext] =
    useState<boolean>(false);

  const { voter } = useGetVoterInfo();
  const { voteContextText } = useGetVoteContextTextFromFile(voteContextUrl);

  const { isMobile, screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const { t } = useTranslation();

  const {
    areFormErrors,
    confirmVote,
    isDirty,
    isVoteLoading,
    registerInput,
    setValue,
    vote,
    canVote,
  } = useVoteActionForm({ previousVote, voteContextHash, voteContextUrl });

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
  }, [previousVote?.url, setVoteContextUrl]);

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
        onClick={confirmVote}
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
    [confirmVote, areFormErrors, vote, isVoteLoading],
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
                date: expiryDate,
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
            title={t("yes")}
            value="yes"
            disabled={isInProgress}
          />
          <Radio
            dataTestId="no-radio"
            isChecked={vote?.toLowerCase() === "no"}
            name="vote"
            register={registerInput}
            setValue={setValue}
            title={t("no")}
            value="no"
            disabled={isInProgress}
          />
          <Radio
            dataTestId="abstain-radio"
            isChecked={vote?.toLowerCase() === "abstain"}
            name="vote"
            register={registerInput}
            setValue={setValue}
            title={t("abstain")}
            value="abstain"
            disabled={isInProgress}
          />
        </Box>
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
                type: "votingPower",
                state: {
                  yesVotes: dRepYesVotes,
                  noVotes: dRepNoVotes,
                  abstainVotes: dRepAbstainVotes,
                  vote: previousVote?.vote,
                } satisfies VotingPowerModalState,
              });
            }}
          >
            {t("govActions.showVotes")}
          </Button>
        )}
        <Typography
          variant="body1"
          sx={{
            textTransform: "uppercase",
            fontSize: "14px",
            color: orange.c400,
            mt: 6,
          }}
        >
          {t("optional")}
        </Typography>
        <Typography
          variant="body2"
          sx={{
            textAlign: "center",
            mt: "5px",
          }}
        >
          {voteContextText
            ? t("govActions.contextAboutYourVote")
            : t("govActions.youCanProvideContext")}
        </Typography>
        {voteContextText && (
          <Box
            sx={{
              display: "flex",
              flexDirection: "column",
              mt: 2,
            }}
          >
            <Typography
              variant="body2"
              sx={{
                fontWeight: 400,
                color: "neutralGray",
                ...(!showWholeVoteContext && {
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  display: "-webkit-box",
                  WebkitBoxOrient: "vertical",
                  WebkitLineClamp: 2,
                }),
              }}
            >
              {voteContextText}
            </Typography>
            <Button
              onClick={() => {
                setShowWholeVoteContext((prev) => !prev);
              }}
              sx={{
                p: 0,
                margin: "0 auto",
                ":hover": {
                  backgroundColor: "transparent",
                },
              }}
              disableRipple
              variant="text"
              data-testid="external-modal-button"
            >
              <Typography
                variant="body2"
                sx={{
                  fontWeight: 400,
                  color: "primaryBlue",
                  borderBottom: "1px solid",
                }}
              >
                {showWholeVoteContext ? t("showLess") : t("showMore")}
              </Typography>
            </Button>
          </Box>
        )}
        <Button
          variant="outlined"
          onClick={() => {
            openModal({
              type: "voteContext",
              state: {
                onSubmit: setVoteContextData,
              } satisfies VoteContextModalState,
            });
          }}
          sx={{
            mt: voteContextText ? "40px" : "12px",
            fontSize:
              screenWidth < 390
                ? "12px"
                : screenWidth < 1036
                ? "14px"
                : screenWidth < 1080
                ? "10.5px"
                : screenWidth < 1480
                ? "11.5px"
                : "14px",
          }}
          data-testid="provide-context-button"
        >
          {voteContextText
            ? t("govActions.provideNewContextAboutYourVote")
            : t("govActions.provideContextAboutYourVote")}
        </Button>
      </Box>
      <Typography
        sx={{
          mb: 2,
          mt: 3,
          textAlign: "center",
          visibility: previousVote?.vote ? "visible" : "hidden",
        }}
        variant="caption"
      >
        {t("govActions.selectDifferentOption")}
      </Typography>
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
        <Button
          data-testid="vote-button"
          variant="contained"
          disabled={
            !vote || previousVote?.vote === vote || (areFormErrors && isDirty)
          }
          isLoading={isVoteLoading}
          onClick={confirmVote}
          size="extraLarge"
        >
          {t("govActions.vote")}
        </Button>
      )}
    </Box>
  );
};
