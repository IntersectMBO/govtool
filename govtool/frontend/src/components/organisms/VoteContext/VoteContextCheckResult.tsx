import { Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";

import { IMAGES, storageInformationErrorModals } from "@consts";
import { Button, Typography } from "@atoms";
import { useScreenDimension, useTranslation, useVoteContextForm } from "@hooks";
import { MetadataValidationStatus } from "@models";

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
  // A validation status gets its own title and explanation, not its raw code.
  const statusModal = errorMessage
    ? storageInformationErrorModals[errorMessage as MetadataValidationStatus]
    : undefined;

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
        {statusModal?.title ??
          errorMessage ??
          "GovTool has processed has your rationale"}
      </Typography>
      <Typography>
        {statusModal?.message ?? errorMessage ?? "You can now proceed to vote."}
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
          {t("govActions.voting.submitVote")}
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
            onClick={() => setStep(4)}
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
