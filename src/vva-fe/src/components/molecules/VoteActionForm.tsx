import { useState, useEffect, useMemo, useCallback } from "react";
import { useLocation } from "react-router-dom";
import { Box, Link } from "@mui/material";

import { Button, Input, LoadingButton, Radio, Typography } from "@atoms";
import { ICONS } from "@consts";
import { useCardano, useModal } from "@context";
import { useScreenDimension, useVoteActionForm, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

export const VoteActionForm = ({
  voteFromEP,
  yesVotes,
  noVotes,
  abstainVotes,
}: {
  voteFromEP?: string;
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
}) => {
  const { state } = useLocation();
  const [isContext, setIsContext] = useState<boolean>(false);
  const { isMobile, screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const { dRep } = useCardano();
  const { t } = useTranslation();

  const {
    setValue,
    control,
    confirmVote,
    vote,
    registerInput,
    errors,
    isDirty,
    clearErrors,
    areFormErrors,
    isLoading,
  } = useVoteActionForm();

  useEffect(() => {
    if (state && state.vote) {
      setValue("vote", state.vote);
    } else if (voteFromEP) {
      setValue("vote", voteFromEP);
    }
  }, [state, voteFromEP, setValue]);

  useEffect(() => {
    clearErrors();
  }, [isContext]);

  const handleContext = useCallback(() => {
    setIsContext((prev) => !prev);
  }, []);

  const renderCancelButton = useMemo(() => {
    return (
      <Button
        data-testid={"cancel-button"}
        onClick={() => setValue("vote", state.vote)}
        variant="outlined"
        size="extraLarge"
        sx={{
          width: "100%",
        }}
      >
        {t("cancel")}
      </Button>
    );
  }, [state]);

  const renderChangeVoteButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid={"change-vote"}
        onClick={confirmVote}
        disabled={
          (!areFormErrors && voteFromEP === vote) ||
          areFormErrors ||
          (!isContext && voteFromEP === vote)
        }
        isLoading={isLoading}
        variant="contained"
        sx={{
          borderRadius: 50,
          textTransform: "none",
          width: "100%",
          height: 48,
        }}
      >
        {t("govActions.changeVote")}
      </LoadingButton>
    );
  }, [confirmVote, areFormErrors, vote]);

  return (
    <Box flex={1} display="flex" flexDirection="column" width={"full"}>
      <Box
        flex={1}
        display="flex"
        flexDirection="column"
        px={screenWidth < 1024 ? 0 : 5}
      >
        <Typography variant="body1">
          {t("govActions.chooseHowToVote")}
        </Typography>
        <Box mt={3}>
          <Box display="flex" flexDirection="row">
            <Radio
              dataTestId="yes-radio"
              isChecked={vote?.toLowerCase() === "yes"}
              name="vote"
              register={registerInput}
              setValue={setValue}
              title={t("yes")}
              value="yes"
            />
            <Box px={1} />
            <Radio
              dataTestId="no-radio"
              isChecked={vote?.toLowerCase() === "no"}
              name="vote"
              register={registerInput}
              setValue={setValue}
              title={t("no")}
              value="no"
            />
          </Box>
          <Box mt={2}>
            <Radio
              dataTestId="abstain-radio"
              isChecked={vote?.toLowerCase() === "abstain"}
              name="vote"
              register={registerInput}
              setValue={setValue}
              title={t("abstain")}
              value="abstain"
            />
          </Box>
        </Box>
        {dRep?.isRegistered && (
          <Button
            data-testid={"show-votes-button"}
            variant="text"
            size="large"
            sx={{ mt: 3 }}
            onClick={() => {
              openModal({
                type: "votingPower",
                state: {
                  yesVotes,
                  noVotes,
                  abstainVotes,
                  vote: state && state.vote ? state.vote : voteFromEP,
                },
              });
            }}
          >
            {t("govActions.showVotes")}
          </Button>
        )}
        <Box
          alignItems="center"
          data-testid="context-button"
          display="flex"
          flexDirection="row"
          flexWrap="wrap"
          justifyContent="center"
          mb={1.5}
          mt={2}
          onClick={handleContext}
        >
          <p
            style={{
              cursor: "pointer",
              fontFamily: "Poppins",
              fontSize: 12,
              fontWeight: 400,
              textAlign: "center",
              margin: 0,
            }}
          >
            {t("govActions.provideContext")}{" "}
            <span style={{ fontSize: 12, fontWeight: 300 }}>
              {t("govActions.optional")}
              <img
                alt="arrow"
                src={ICONS.arrowDownIcon}
                style={{
                  marginBottom: 2,
                  marginLeft: 12,
                  transform: `rotate(${!isContext ? "180deg" : "0"})`,
                }}
              />
            </span>
          </p>
        </Box>
        {isContext && (
          <Box display={"flex"} flexDirection={"column"} flex={1}>
            <Input
              control={control}
              dataTestId="url-input"
              errorMessage={errors.url?.message}
              formFieldName="url"
              placeholder={t("forms.urlWithContextPlaceholder")}
              width={"100%"}
            />
            <Input
              control={control}
              dataTestId="hash-input"
              errorMessage={errors.hash?.message}
              formFieldName="hash"
              placeholder={t("forms.hashPlaceholder")}
              width={"100%"}
            />
            <Link
              data-testid={"how-to-create-link"}
              onClick={() =>
                openInNewTab(
                  "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor"
                )
              }
              mb={isMobile ? 2 : 8}
              sx={{ cursor: "pointer" }}
              textAlign={"center"}
              visibility={!isContext ? "hidden" : "visible"}
            >
              <Typography color="primary" fontWeight={400} variant="body2">
                {t("forms.howCreateUrlAndHash")}
              </Typography>
            </Link>
          </Box>
        )}
      </Box>
      <Typography
        sx={{
          mb: 1,
          textAlign: "center",
          visibility: state?.vote || voteFromEP ? "visible" : "hidden",
        }}
        variant="caption"
      >
        {t("govActions.selectDifferentOption")}
      </Typography>
      {(state?.vote && state?.vote !== vote) ||
      (voteFromEP && voteFromEP !== vote) ? (
        <Box
          display={"flex"}
          flexDirection={isMobile ? "column" : "row"}
          justifyContent={"space-between"}
        >
          {isMobile ? renderChangeVoteButton : renderCancelButton}
          <Box px={1} py={isMobile ? 1.5 : 0} />
          {isMobile ? renderCancelButton : renderChangeVoteButton}
        </Box>
      ) : (
        <Button
          data-testid={"vote-button"}
          variant="contained"
          disabled={
            !vote ||
            state?.vote === vote ||
            (isContext && areFormErrors && isDirty) ||
            voteFromEP === vote
          }
          onClick={confirmVote}
          size="extraLarge"
          sx={{
            width: "100%",
          }}
        >
          {t("govActions.vote")}
        </Button>
      )}
    </Box>
  );
};
