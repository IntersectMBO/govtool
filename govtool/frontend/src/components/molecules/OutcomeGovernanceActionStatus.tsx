import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { OutcomeStatus } from "@models";

import { OutcomeStatusChip } from "./OutcomeStatusChip";

type OutcomeGovernanceActionStatusProps = {
  status: OutcomeStatus;
  actionId: string;
  isCard?: boolean;
};

/** Every status the action has passed through: "Ratified" and "Enacted" can both show. */
export const OutcomeGovernanceActionStatus = ({
  status,
  actionId,
  isCard = true,
}: OutcomeGovernanceActionStatusProps) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const getStatusLabels = () => {
    const ratified = !!status.ratified_epoch;
    const enacted = !!status.enacted_epoch;
    const dropped = !!status.dropped_epoch;
    const expired = !!status.expired_epoch;

    if (!ratified && !enacted && !dropped && !expired) {
      return [t("outcome.status.inProgress")];
    }
    if (ratified && enacted) {
      return [t("outcome.status.ratified"), t("outcome.status.enacted")];
    }
    if (ratified) return [t("outcome.status.ratified")];
    if (enacted) return [t("outcome.status.enacted")];
    if (expired && dropped) {
      return [t("outcome.status.expired"), t("outcome.status.dropped")];
    }
    if (dropped) return [t("outcome.status.dropped")];
    return [t("outcome.status.expired")];
  };

  return (
    <Box
      data-testid={`${actionId}-status`}
      display="flex"
      justifyContent={isCard ? "space-between" : undefined}
      gap={isCard ? 0 : isMobile ? 3 : 8.65}
      width="100%"
      alignItems="center"
      flexWrap="wrap"
      sx={{
        "& > .MuiTypography-root": {
          marginBottom: "4px",
        },
      }}
    >
      <Typography
        sx={{
          fontSize: isCard ? 12 : 14,
          color: "neutralGray",
          fontWeight: isCard ? 500 : 600,
        }}
      >
        {t("outcome.status.title")}
      </Typography>
      <Box display="flex" flexDirection="row" gap={2}>
        {getStatusLabels().map((label) => (
          <OutcomeStatusChip key={label} label={label} />
        ))}
      </Box>
    </Box>
  );
};
