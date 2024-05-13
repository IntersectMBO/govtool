import { Box, Typography } from "@mui/material";

import { correctAdaFormat } from "@/utils";
import { useMediaQuery } from "@/hooks";

import { Tooltip } from "./Tooltip";

import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

export const VotingPowerChips = ({
  votingPower,
  tooltipHeading,
  tooltipParagraphOne,
  tooltipParagraphTwo,
}) => {
  const { tablet } = useMediaQuery();
  return (
    <Box
      sx={[
        style.container,
        {
          border: tablet ? 0 : 2,
          height: tablet ? 24 : 16,
          maxHeight: tablet ? "20px" : "9px",
          py: tablet ? "14px" : "9.5px",
        },
      ]}
    >
      {(tooltipHeading || tooltipParagraphOne || tooltipParagraphTwo) && (
        <Tooltip
          heading={tooltipHeading}
          paragraphOne={tooltipParagraphOne}
          paragraphTwo={tooltipParagraphTwo}
        >
          <InfoOutlinedIcon
            style={{
              color: "#ADAEAD",
              marginRight: "12px",
            }}
            fontSize="small"
          />
        </Tooltip>
      )}
      {tablet && (
        <Typography
          color="grays.400"
          sx={{ mr: 1.5, whiteSpace: "nowrap" }}
          variant="titleS"
        >
          Voting power:
        </Typography>
      )}
      <Typography
        color="white"
        variant="titleSemiBig"
        sx={{ whiteSpace: "nowrap" }}
      >
        ₳ {correctAdaFormat(votingPower) ?? 0}
      </Typography>
    </Box>
  );
};

const style = {
  container: {
    ml: "24px",
    alignItems: "center",
    backgroundColor: "darkPurple",
    borderColor: "arcticWhite",
    borderRadius: 100,
    display: "flex",
    px: "16px",
  },
};
