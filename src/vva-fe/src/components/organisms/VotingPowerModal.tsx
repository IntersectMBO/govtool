import { Box } from "@mui/material";

import { ModalContents, ModalWrapper, Typography, VotePill } from "@atoms";
import { useModal } from "@context";
import { correctAdaFormat } from "@utils";
import { Vote } from "@/models";
import { useScreenDimension } from "@/hooks";
import { usei18n } from "@/translations";

export interface VotingPowerModalState {
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
  vote?: string;
}

export function VotingPowerModal() {
  const { state } = useModal<VotingPowerModalState>();
  const { isMobile } = useScreenDimension();
  const { t } = usei18n();

  const VOTES = [
    { title: "yes", vote: state?.yesVotes },
    { title: "abstain", vote: state?.abstainVotes },
    { title: "no", vote: state?.noVotes },
  ];

  return (
    <ModalWrapper
      dataTestId="external-link-modal"
      sx={{ maxWidth: "372px", paddingBottom: 6 }}
    >
      <ModalContents>
        <Box alignItems="center">
          <Typography sx={{ textAlign: "center" }} variant="title2">
            {t("modals.votingPower.govActionsVotes")}
          </Typography>
          <Typography
            fontWeight={400}
            sx={{ mt: 1, mb: 3, textAlign: "center" }}
            variant="body2"
          >
            {t("modals.votingPower.votesSubmittedByDReps")}
          </Typography>
          {VOTES.map((vote, index) => (
            <Box
              border={state?.vote?.toLocaleLowerCase() === vote.title ? 1 : 0}
              px={isMobile ? 1.5 : 3}
              py={
                state?.vote?.toLocaleLowerCase() === vote.title
                  ? isMobile
                    ? 2
                    : 3
                  : 0
              }
              borderColor={"lightBlue"}
              sx={{
                alignItems: "center",
                display: "flex",
                marginBottom: index + 1 === VOTES.length ? 0 : "24px",
                borderRadius: 12,
                position: "relative",
              }}
            >
              {state?.vote?.toLocaleLowerCase() === vote.title ? (
                <Typography
                  fontWeight={500}
                  sx={{
                    color: "#9792B5",
                    backgroundColor: "#fbfbfb",
                    position: "absolute",
                    top: -8,
                    px: 0.87,
                  }}
                  variant="caption"
                >
                  {t("modals.votingPower.yourVote")}
                </Typography>
              ) : null}
              <VotePill
                vote={vote.title as Vote}
                width={isMobile ? 50 : 120}
                maxWidth={82}
              />
              <Typography
                fontWeight={500}
                sx={{ marginLeft: "12px", whiteSpace: "nowrap" }}
                variant="body1"
              >
                ₳ {correctAdaFormat(vote.vote)}
              </Typography>
            </Box>
          ))}
        </Box>
      </ModalContents>
    </ModalWrapper>
  );
}
