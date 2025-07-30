import { useState, useEffect, useMemo, Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";
import { Trans } from "react-i18next";

import { Button, Radio, Typography } from "@atoms";
import { fadedPurple } from "@/consts";
import { useModal } from "@context";
import {
  useScreenDimension,
  useVoteActionForm,
  useTranslation,
  useGetVoterInfo,
  useGetVoteContextTextFromFile,
} from "@hooks";
import { formatDisplayDate } from "@utils";
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
  const [voteContextHash, setVoteContextHash] = useState<string | undefined>();
  const [voteContextUrl, setVoteContextUrl] = useState<string | undefined>();
  const [showWholeVoteContext, setShowWholeVoteContext] =
    useState<boolean>(false);

  const { voter } = useGetVoterInfo();
  const { voteContextText } = useGetVoteContextTextFromFile(voteContextUrl , voteContextHash);

  const { isMobile, screenWidth } = useScreenDimension();
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
  } = useVoteActionForm({ previousVote, voteContextHash, voteContextUrl, closeModal });

  const handleVoteClick = (isVoteChanged:boolean) => {
    openModal({
      type: "voteContext",
      state: {
        onSubmit: (url, hash) => {
          setVoteContextUrl(url);
          setVoteContextHash(hash ?? undefined);
          confirmVote(vote as Vote, url, hash);
          setVoteContextData(url , hash);
        },
        vote: vote as Vote,
        confirmVote,
        previousRationale : isVoteChanged?undefined:voteContextText
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
      onClick={ ()=>handleVoteClick(true)}
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

  useEffect(()=>{
    console.log(previousVote?.metadataHash , voteContextHash)
  } , [previousVote?.metadataHash , voteContextHash])
  
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
        {voteContextText && (
          <>
          <Typography sx={{fontSize : "14px" , fontWeight : 500}}>{t("govActions.yourVoteRationale")}</Typography>
          <Box
            sx={{
              display: "flex",
              flexDirection: "column",
              justifyContent : "space-between",
              width: "100%",
              mt: 2,
            }}
          >
          {voteContextText && (
          <Box
            sx={{
              position: "relative",
              width: "100%",
              mt: 2,
              border: !showWholeVoteContext ? "1px solid #E1E1E1" : "none",
              borderRadius: "4px",
              backgroundColor: !showWholeVoteContext ? fadedPurple.c50 : "transparent",
              padding: 2,
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
            >
      {voteContextText}
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

        <Box sx={{ mt: 4 }}>
        </Box>
      </Box>
      {previousVote?.vote && previousVote?.vote !== vote  ? (
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
          onClick={()=>handleVoteClick(false)}
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
