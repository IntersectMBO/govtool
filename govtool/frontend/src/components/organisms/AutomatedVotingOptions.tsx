import { useState } from "react";
import { Box } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import { ICONS } from "@consts";
import { useTranslation } from "@hooks";
import { AutomatedVotingCard } from "@molecules";

export const AutomatedVotingOptions = () => {
  const [isOpen, setIsOpen] = useState<boolean>(false);
  const { t } = useTranslation();

  const handleOpen = () => {
    setIsOpen((prev) => !prev);
  };

  return (
    <Box
      sx={{
        bgcolor: "#D6E2FF80",
        border: "1px solid #F7F9FB",
        borderRadius: 3,
        boxShadow: `2px 2px 15px 0px #2F62DC47`,
        display: "flex",
        flexDirection: "column",
        padding: "12px 24px",
      }}
    >
      <Box
        onClick={handleOpen}
        sx={{
          alignItems: "center",
          cursor: "pointer",
          display: "flex",
          flex: 1,
          justifyContent: "space-between",
        }}
      >
        <Typography>{t("dRepDirectory.automatedVotingOptions")}</Typography>
        <img
          alt="arrow"
          src={ICONS.arrowDownIcon}
          style={{
            transform: `rotate(${isOpen ? "180deg" : "0"})`,
          }}
        />
      </Box>
      {isOpen ? (
        <>
          <Spacer y={2} />
          <AutomatedVotingCard
            description={t("dRepDirectory.abstainCardDescription")}
            onClickDelegate={() => {}}
            onClickInfo={() => {}}
            title={t("dRepDirectory.abstainCardTitle")}
            votingPower="99,111,111"
          />
          <Spacer y={2} />
          <AutomatedVotingCard
            description={t("dRepDirectory.noConfidenceDescription")}
            onClickDelegate={() => {}}
            onClickInfo={() => {}}
            title={t("dRepDirectory.noConfidenceTitle")}
            votingPower="99,111,111"
          />
        </>
      ) : null}
    </Box>
  );
};
