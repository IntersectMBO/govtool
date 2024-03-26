import { Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";

import { IMAGES } from "@consts";
import { Button, Typography } from "@atoms";
import { useScreenDimension, useTranslation, useVoteContextForm } from "@hooks";

type VoteContextCheckResultProps = {
  submitVoteContext: () => void;
  closeModal: () => void;
  setStep: Dispatch<SetStateAction<number>>;
  errorMessage?: string;
};

export const VoteContextCheckResult = ({
  submitVoteContext,
  closeModal,
  setStep,
  errorMessage,
}: VoteContextCheckResultProps) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  const { watch } = useVoteContextForm();
  const isContinueDisabled = !watch("voteContextText");

  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
      }}
    >
      <img
        alt="Status icon"
        src={errorMessage ? IMAGES.warningImage : IMAGES.successImage}
        style={{ height: "84px", margin: "0 auto", width: "84px" }}
      />
      <Typography
        variant="title2"
        sx={{
          lineHeight: "34px",
          mb: 1,
          mt: 3,
        }}
      >
        {errorMessage ? "Data validation failed" : "Success"}
      </Typography>
      <Typography variant="body1" sx={{ fontWeight: 400, mb: 2 }}>
        {errorMessage ?? "Data check has been successful"}
      </Typography>
      {!errorMessage ? (
        <Button
          data-testid="go-to-vote-modal-button"
          onClick={submitVoteContext}
          sx={{
            borderRadius: 50,
            margin: "0 auto",
            padding: "10px 26px",
            textTransform: "none",
            marginTop: "38px",
            width: "100%",
          }}
          variant="contained"
        >
          {t("govActions.goToVote")}
        </Button>
      ) : (
        <Box
          sx={{
            display: "flex",
            justifyContent: "space-between",
            marginTop: "40px",
            width: "100%",
            ...(isMobile && { flexDirection: "column-reverse", gap: 3 }),
          }}
        >
          <Button
            data-testid="go-back-modal-button"
            onClick={() => setStep(3)}
            size="large"
            sx={{
              width: isMobile ? "100%" : "154px",
            }}
            variant="outlined"
          >
            {t("goBack")}
          </Button>
          <Button
            data-testid="close-modal-button"
            disabled={isContinueDisabled}
            onClick={closeModal}
            size="large"
            sx={{
              width: isMobile ? "100%" : "154px",
            }}
            variant="contained"
          >
            {t("close")}
          </Button>
        </Box>
      )}
    </Box>
  );
};
