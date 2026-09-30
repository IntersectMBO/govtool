import { Box } from "@mui/material";

import { useGetOutcomeNetworkMetrics, useTranslation } from "@hooks";
import { OutcomeGovernanceAction } from "@models";

import { GovernanceActionCardElement } from "./GovernanceActionCardElement";

type OutcomeHardForkDetailsProps = {
  action: OutcomeGovernanceAction;
  prevGovActionId: string | null;
};

/** Protocol version before and after a HardForkInitiation, and the action it follows. */
export const OutcomeHardForkDetails = ({
  action,
  prevGovActionId,
}: OutcomeHardForkDetailsProps) => {
  const { epochParams } = useGetOutcomeNetworkMetrics(action);
  const { t } = useTranslation();

  return (
    <Box display="flex" flexDirection="column" gap={3}>
      <GovernanceActionCardElement
        label={t("outcome.currentVersion")}
        text={
          epochParams
            ? `${epochParams.protocol_major}.${epochParams.protocol_minor}`
            : "-"
        }
        dataTestId="hard-fork-current-version"
        marginBottom={0}
      />
      <GovernanceActionCardElement
        label={t("outcome.proposedVersion")}
        text={
          action.description
            ? `${action.description.major}.${action.description.minor}`
            : "-"
        }
        dataTestId="hard-fork-proposed-version"
        marginBottom={0}
      />
      <GovernanceActionCardElement
        label={t("outcome.prevGovernanceActionId")}
        text={prevGovActionId ?? "-"}
        dataTestId="previous-governance-action-id"
        textVariant="longText"
        isCopyButton={!!prevGovActionId}
        marginBottom={0}
      />
    </Box>
  );
};
