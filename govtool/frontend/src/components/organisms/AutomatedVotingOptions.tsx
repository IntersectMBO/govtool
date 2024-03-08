import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
} from "@mui/material";

import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { useTranslation } from "@hooks";
import { AutomatedVotingCard } from "@molecules";

export const AutomatedVotingOptions = () => {
  const { t } = useTranslation();

  return (
    <Accordion
      elevation={3}
      sx={(theme) => ({
        bgcolor: `${theme.palette.lightBlue}80`,
        border: `1px solid ${theme.palette.neutralWhite}`,
      })}
    >
      <AccordionSummary
        expandIcon={<img alt="arrow" src={ICONS.arrowDownIcon} />}
        sx={{ borderRadius: 3, px: { xxs: 2, md: 3 } }}
      >
        <Typography>{t("dRepDirectory.automatedVotingOptions")}</Typography>
      </AccordionSummary>
      <AccordionDetails sx={{ p: { xxs: 2, md: 3 }, pt: { xxs: 0, md: 0 } }}>
        <Box
          sx={{
            display: "flex",
            flexDirection: "column",
            gap: 2,
          }}
        >
          <AutomatedVotingCard
            description={t("dRepDirectory.abstainCardDescription")}
            onClickDelegate={() => {}}
            onClickInfo={() => {}}
            title={t("dRepDirectory.abstainCardTitle")}
            votingPower="99,111,111"
          />
          <AutomatedVotingCard
            description={t("dRepDirectory.noConfidenceDescription")}
            onClickDelegate={() => {}}
            onClickInfo={() => {}}
            title={t("dRepDirectory.noConfidenceTitle")}
            votingPower="99,111,111"
          />
        </Box>
      </AccordionDetails>
    </Accordion>
  );
};
