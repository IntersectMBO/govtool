import {
  useState,
  useEffect,
  useMemo,
  useCallback,
  Dispatch,
  SetStateAction,
} from "react";
import { useLocation } from "react-router-dom";
import { Box, Link } from "@mui/material";

import { Button, LoadingButton, Radio, Spacer, Typography } from "@atoms";
import { ICONS } from "@consts";
import { useModal } from "@context";
import {
  useScreenDimension,
  useVoteActionForm,
  useTranslation,
  useGetVoterInfo,
} from "@hooks";
import { openInNewTab } from "@utils";

import { Trans } from "react-i18next";
import { ControlledField } from "../organisms";

// TODO: Decide with BE on how cast votes will be implemented
// and adjust accordingly the component below (UI is ready).
const castVoteDate = undefined;
const castVoteChangeDeadline = "20.06.2024 (Epoch 445)";

type VoteActionFormProps = {
  setIsVoteSubmitted: Dispatch<SetStateAction<boolean>>;
  voteFromEP?: string;
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
  isInProgress?: boolean;
};

export const VoteActionForm = ({
  setIsVoteSubmitted,
  voteFromEP,
  yesVotes,
  noVotes,
  abstainVotes,
  isInProgress,
}: VoteActionFormProps) => {
  const [isContext, setIsContext] = useState<boolean>(false);
  const { state } = useLocation();
  const { isMobile } = useScreenDimension();
  const { openModal } = useModal();
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();

  const {
    areFormErrors,
    clearErrors,
    confirmVote,
    control,
    errors,
    isDirty,
    isVoteLoading,
    registerInput,
    setValue,
    vote,
  } = useVoteActionForm();

  useEffect(() => {
    if (state && state.vote) {
      setValue("vote", state.vote);
      setIsVoteSubmitted(true);
    } else if (voteFromEP) {
      setValue("vote", voteFromEP);
      setIsVoteSubmitted(true);
    }
  }, [state, voteFromEP, setValue]);

  useEffect(() => {
    clearErrors();
  }, [isContext]);

  const handleContext = useCallback(() => {
    setIsContext((prev) => !prev);
  }, []);

  const renderCancelButton = useMemo(
    () => (
      <Button
        data-testid="cancel-button"
        onClick={() => setValue("vote", state.vote)}
        variant="outlined"
        size="extraLarge"
        sx={{
          width: "100%",
        }}
      >
        {t("cancel")}
      </Button>
    ),
    [state],
  );

  const renderChangeVoteButton = useMemo(
    () => (
      <LoadingButton
        data-testid="change-vote"
        onClick={confirmVote}
        disabled={
          (!areFormErrors && voteFromEP === vote) ||
          areFormErrors ||
          (!isContext && voteFromEP === vote)
        }
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
      </LoadingButton>
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
        {castVoteDate ? (
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
                values={{ vote: vote?.toLocaleUpperCase(), date: castVoteDate }}
                components={[<span style={{ fontWeight: 600 }} key="0" />]}
              />
            </Typography>
            <Typography
              variant="caption"
              sx={{ lineHeight: "18px", alignSelf: "start" }}
            >
              {t("govActions.castVoteDeadline", {
                date: castVoteChangeDeadline,
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
        {/* TODO: Change below into new voting context */}
        <Box
          alignItems="center"
          data-testid="context-button"
          display="flex"
          flexDirection="row"
          flexWrap="wrap"
          justifyContent="center"
          mb="15px"
          mt="58px"
          onClick={handleContext}
        >
          <p
            style={{
              cursor: "pointer",
              fontFamily: "Poppins",
              fontSize: 12,
              fontWeight: 300,
              lineHeight: "18px",
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
          <Box
            display="flex"
            flexDirection="column"
            flex={1}
            sx={{
              alignSelf: "stretch",
            }}
          >
            <ControlledField.Input
              {...{ control, errors }}
              dataTestId="url-input"
              name="url"
              placeholder={t("forms.urlWithContextPlaceholder")}
            />
            <Spacer y={1.5} />
            <ControlledField.Input
              {...{ control, errors }}
              dataTestId="hash-input"
              name="hash"
              placeholder={t("forms.hashPlaceholder")}
            />
            <Link
              data-testid="how-to-create-link"
              onClick={() =>
                openInNewTab(
                  "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor",
                )
              }
              mt="12px"
              mb={isMobile ? 2 : 6}
              sx={{ cursor: "pointer" }}
              textAlign="center"
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
          display="flex"
          flexDirection={isMobile ? "column" : "row"}
          justifyContent="space-between"
        >
          {isMobile ? renderChangeVoteButton : renderCancelButton}
          <Box px={1} py={isMobile ? 1.5 : 0} />
          {isMobile ? renderCancelButton : renderChangeVoteButton}
        </Box>
      ) : (
        <LoadingButton
          data-testid="vote-button"
          variant="contained"
          disabled={
            !vote ||
            state?.vote === vote ||
            (isContext && areFormErrors && isDirty) ||
            voteFromEP === vote
          }
          isLoading={isVoteLoading}
          onClick={confirmVote}
          size="extraLarge"
        >
          {t("govActions.vote")}
        </LoadingButton>
      )}
    </Box>
  );
};
