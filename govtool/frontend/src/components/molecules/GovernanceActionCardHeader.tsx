import { Box, Skeleton } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Tooltip, Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { getMetadataDataMissingStatusTranslation } from "@/utils";
import { MetadataValidationStatus } from "@/models";

type GovernanceActionCardHeaderProps = {
  title?: string;
  isDataMissing?: MetadataValidationStatus;
  isValidating?: boolean;
  dataTestId?: string;
};

export const GovernanceActionCardHeader = ({
  title,
  isDataMissing,
  isValidating,
  dataTestId = "governance-action-card-header",
}: GovernanceActionCardHeaderProps) => {
  const { t } = useTranslation();

  return (
    <Box
      sx={{
        display: "flex",
        alignItems: "center",
        mb: "20px",
        overflow: "hidden",
      }}
      data-testid={dataTestId}
    >
      {isValidating ? (
        <Skeleton height="24px" width="100px" variant="rounded" />
      ) : (
        <Typography
          sx={{
            fontSize: 18,
            fontWeight: 600,
            lineHeight: "24px",
            display: "-webkit-box",
            WebkitBoxOrient: "vertical",
            WebkitLineClamp: 2,
            wordBreak: "break-word",
            ...(isDataMissing && { color: "errorRed" }),
          }}
        >
          {(isDataMissing &&
            getMetadataDataMissingStatusTranslation(
              isDataMissing as MetadataValidationStatus,
            )) ||
            title}
        </Typography>
      )}
      {isDataMissing && typeof isDataMissing === "string" && (
        <Tooltip
          heading={getMetadataDataMissingStatusTranslation(
            isDataMissing as MetadataValidationStatus,
          )}
          paragraphOne={t("govActions.dataMissingTooltipExplanation")}
          placement="bottom-end"
          arrow
        >
          <InfoOutlinedIcon
            style={{
              color: "#ADAEAD",
            }}
            sx={{ ml: 0.7 }}
            fontSize="small"
          />
        </Tooltip>
      )}
    </Box>
  );
};
