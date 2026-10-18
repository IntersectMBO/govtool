import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { GovernanceActionStatus as GovernanceActionLifecycle } from "@models";

import { GovernanceActionStatusChip } from "./GovernanceActionStatusChip";

type GovernanceActionStatusProps = {
  status: GovernanceActionLifecycle;
  actionId: string;
  isCard?: boolean;
};

/** Every status the action has passed through: "Ratified" and "Enacted" can both show. */
export const GovernanceActionStatus = ({
  status,
  actionId,
  isCard = true,
}: GovernanceActionStatusProps) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const getStatusLabels = () => {
    const ratified = !!status.ratified_epoch;
    const enacted = !!status.enacted_epoch;
    const dropped = !!status.dropped_epoch;
    const expired = !!status.expired_epoch;

    if (!ratified && !enacted && !dropped && !expired) {
      return [t("actionRecord.status.inProgress")];
    }
    if (ratified && enacted) {
      return [t("actionRecord.status.ratified"), t("actionRecord.status.enacted")];
    }
    if (ratified) return [t("actionRecord.status.ratified")];
    if (enacted) return [t("actionRecord.status.enacted")];
    if (expired && dropped) {
      return [t("actionRecord.status.expired"), t("actionRecord.status.dropped")];
    }
    if (dropped) return [t("actionRecord.status.dropped")];
    return [t("actionRecord.status.expired")];
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
        {t("actionRecord.status.title")}
      </Typography>
      <Box display="flex" flexDirection="row" gap={2}>
        {getStatusLabels().map((label) => (
          <GovernanceActionStatusChip key={label} label={label} />
        ))}
      </Box>
    </Box>
  );
};
