import { useState } from "react";
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
  Chip,
} from "@mui/material";

import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { PendingTransaction } from "@context";
import { useTranslation } from "@hooks";
import { AutomatedVotingCard } from "@molecules";
import { openInNewTab } from "@/utils";

type AutomatedVotingOptionsProps = {
  currentDelegation?: string | null;
  delegate: (delegateTo: string) => void;
  votingPower: string;
  delegationInProgress?: string;
  isConnected?: boolean;
  isDelegationLoading?: boolean;
  pendingTransaction?: PendingTransaction;
};

export const AutomatedVotingOptions = ({
  currentDelegation,
  delegate,
  delegationInProgress,
  isConnected,
  isDelegationLoading,
  pendingTransaction,
  votingPower,
}: AutomatedVotingOptionsProps) => {
  const { t } = useTranslation();

  const [isOpen, setIsOpen] = useState<boolean>(false);

  // TODO: Change to certain automated voted option if available
  const onClickInfo = () => openInNewTab("https://docs.sanchogov.tools/");

  return (
    <Accordion
      data-testid="automated-voting-options-accordion"
      elevation={3}
      expanded={isOpen}
      onChange={(_, isExpanded) => setIsOpen(isExpanded)}
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
        {currentDelegation && !isOpen && (
          // TODO this Chip is temporary, since there were no design for this case
          <Chip
            color="primary"
            label={
              currentDelegation === "drep_always_abstain"
                ? "Abstain"
                : "No confidence"
            }
            sx={{
              backgroundColor: (theme) => theme.palette.neutralWhite,
              fontWeight: 400,
              ml: 2,
              textTransform: "uppercase",
            }}
          />
        )}
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
            inProgress={delegationInProgress === "abstain"}
            isConnected={isConnected}
            isDelegateLoading={isDelegationLoading}
            isSelected={currentDelegation === "drep_always_abstain"}
            onClickDelegate={() => delegate("abstain")}
            onClickInfo={onClickInfo}
            title={t("dRepDirectory.abstainCardTitle")}
            votingPower={votingPower}
            transactionId={
              pendingTransaction?.delegate?.resourceId === "abstain"
                ? pendingTransaction?.delegate?.transactionHash
                : undefined
            }
          />
          <AutomatedVotingCard
            description={t("dRepDirectory.noConfidenceDescription")}
            inProgress={delegationInProgress === "no confidence"}
            isConnected={isConnected}
            isDelegateLoading={isDelegationLoading}
            isSelected={currentDelegation === "drep_always_no_confidence"}
            onClickDelegate={() => delegate("no confidence")}
            onClickInfo={onClickInfo}
            title={t("dRepDirectory.noConfidenceTitle")}
            votingPower={votingPower}
            transactionId={
              pendingTransaction?.delegate?.resourceId === "no confidence"
                ? pendingTransaction?.delegate?.transactionHash
                : undefined
            }
          />
        </Box>
      </AccordionDetails>
    </Accordion>
  );
};
