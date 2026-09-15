import { useEffect, useState } from "react";
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
} from "@mui/material";

import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { PendingTransaction } from "@context";
import { useGetNetworkTotalStake, useTranslation } from "@hooks";
import { AutomatedVotingCard } from "@molecules";
import { correctDRepDirectoryFormat, openInNewTab } from "@/utils";
import {
  AutomatedVotingOptionCurrentDelegation,
  AutomatedVotingOptionDelegationId,
} from "@/types/automatedVotingOptions";
import { LINKS } from "@/consts/links";

type AutomatedVotingOptionsProps = {
  currentDelegation?: string | null;
  delegate: (delegateTo: string) => void;
  votingPower: string;
  delegationInProgress?: string;
  isConnected?: boolean;
  isDelegationLoading?: string | null;
  pendingTransaction?: PendingTransaction;
  txHash?: string | null;
};

export const AutomatedVotingOptions = ({
  currentDelegation,
  delegate,
  delegationInProgress,
  isConnected,
  isDelegationLoading,
  pendingTransaction,
  votingPower,
  txHash,
}: AutomatedVotingOptionsProps) => {
  const { t } = useTranslation();
  const { networkTotalStake, fetchNetworkTotalStake } =
    useGetNetworkTotalStake();

  const [isOpen, setIsOpen] = useState<boolean>(false);

  const isDelegatedToAbstain =
    currentDelegation ===
    AutomatedVotingOptionCurrentDelegation.drep_always_abstain;
  const isDelegationToAbstainInProgress =
    delegationInProgress === AutomatedVotingOptionDelegationId.abstain;
  const isDelegatedToNoConfidence =
    currentDelegation ===
    AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence;
  const isDelegationToNoConfidenceInProgress =
    delegationInProgress === AutomatedVotingOptionDelegationId.no_confidence;

  useEffect(() => {
    fetchNetworkTotalStake();
  }, []);

  useEffect(() => {
    const shouldBeSetOpen =
      isDelegatedToAbstain ||
      isDelegatedToNoConfidence ||
      isDelegationToAbstainInProgress ||
      isDelegationToNoConfidenceInProgress;

    setIsOpen(shouldBeSetOpen);
  }, [
    isDelegatedToAbstain,
    isDelegatedToNoConfidence,
    isDelegationToAbstainInProgress,
    isDelegationToNoConfidenceInProgress,
  ]);

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
      </AccordionSummary>
      <AccordionDetails
        sx={{ p: { xxs: 2, md: 3 }, pt: { xxs: 0, md: 0 } }}
        data-testid="delegation-options-dropdown"
      >
        <Box
          sx={{
            display: "flex",
            flexDirection: "column",
            gap: 2,
          }}
        >
          <AutomatedVotingCard
            dataTestId="abstain-delegation-card"
            description={
              isDelegatedToAbstain
                ? t("dRepDirectory.delegatedToAbstainDescription")
                : t("dRepDirectory.abstainCardDefaultDescription")
            }
            inProgress={isDelegationToAbstainInProgress}
            isConnected={isConnected}
            isDelegateLoading={
              isDelegationLoading === AutomatedVotingOptionDelegationId.abstain
            }
            isSelected={isDelegatedToAbstain}
            onClickDelegate={() =>
              delegate(AutomatedVotingOptionDelegationId.abstain)
            }
            onClickInfo={() => openInNewTab(LINKS.ABSTAIN_FROM_EVERY_VOTE)}
            title={
              isDelegatedToAbstain
                ? t("dRepDirectory.delegatedToAbstainTitle", {
                    ada: votingPower,
                  })
                : t("dRepDirectory.abstainCardDefaultTitle")
            }
            votingPower={
              networkTotalStake
                ? correctDRepDirectoryFormat(
                    networkTotalStake?.alwaysAbstainVotingPower,
                  )
                : ""
            }
            transactionId={
              pendingTransaction?.delegate?.resourceId ===
              AutomatedVotingOptionDelegationId.abstain
                ? pendingTransaction?.delegate?.transactionHash
                : isDelegatedToAbstain
                ? txHash
                : undefined
            }
          />
          <AutomatedVotingCard
            dataTestId="no-confidence-delegation-card"
            description={
              isDelegatedToNoConfidence
                ? t("dRepDirectory.delegatedToNoConfidenceDescription")
                : t("dRepDirectory.noConfidenceDefaultDescription")
            }
            inProgress={isDelegationToNoConfidenceInProgress}
            isConnected={isConnected}
            isDelegateLoading={
              isDelegationLoading ===
              AutomatedVotingOptionDelegationId.no_confidence
            }
            isSelected={isDelegatedToNoConfidence}
            onClickDelegate={() =>
              delegate(AutomatedVotingOptionDelegationId.no_confidence)
            }
            onClickInfo={() =>
              openInNewTab(LINKS.SIGNAL_NO_CONFIDENCE_ON_EVERY_VOTE)
            }
            title={
              isDelegatedToNoConfidence
                ? t("dRepDirectory.delegatedToNoConfidenceTitle", {
                    ada: votingPower,
                  })
                : t("dRepDirectory.noConfidenceDefaultTitle")
            }
            votingPower={
              networkTotalStake
                ? correctDRepDirectoryFormat(
                    networkTotalStake?.alwaysNoConfidenceVotingPower,
                  )
                : ""
            }
            transactionId={
              pendingTransaction?.delegate?.resourceId ===
              AutomatedVotingOptionDelegationId.no_confidence
                ? pendingTransaction?.delegate?.transactionHash
                : isDelegatedToNoConfidence
                ? txHash
                : undefined
            }
          />
        </Box>
      </AccordionDetails>
    </Accordion>
  );
};
