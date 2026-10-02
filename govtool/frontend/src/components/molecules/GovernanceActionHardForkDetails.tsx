import { Box } from "@mui/material";

import { useGetGovernanceActionEpochParams, useTranslation } from "@hooks";
import { GovernanceActionRecord } from "@models";

import { GovernanceActionCardElement } from "./GovernanceActionCardElement";

type GovernanceActionHardForkDetailsProps = {
  action: GovernanceActionRecord;
  prevGovActionId: string | null;
};

/** Protocol version before and after a HardForkInitiation, and the action it follows. */
export const GovernanceActionHardForkDetails = ({
  action,
  prevGovActionId,
}: GovernanceActionHardForkDetailsProps) => {
  const { epochParams } = useGetGovernanceActionEpochParams(action);
  const { t } = useTranslation();

  return (
    <Box display="flex" flexDirection="column" gap={3}>
      <GovernanceActionCardElement
        label={t("actionRecord.currentVersion")}
        text={
          epochParams
            ? `${epochParams.protocol_major}.${epochParams.protocol_minor}`
            : "-"
        }
        dataTestId="hard-fork-current-version"
        marginBottom={0}
      />
      <GovernanceActionCardElement
        label={t("actionRecord.proposedVersion")}
        text={
          action.description
            ? `${action.description.major}.${action.description.minor}`
            : "-"
        }
        dataTestId="hard-fork-proposed-version"
        marginBottom={0}
      />
      <GovernanceActionCardElement
        label={t("actionRecord.prevGovernanceActionId")}
        text={prevGovActionId ?? "-"}
        dataTestId="previous-governance-action-id"
        textVariant="longText"
        isCopyButton={!!prevGovActionId}
        marginBottom={0}
      />
    </Box>
  );
};
