import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Tooltip, Typography } from "@atoms";
import { useTranslation } from "@hooks";

type GovernanceActionCardHeaderProps = {
  title?: string;
  isDataMissing: string | boolean;
};

export const GovernanceActionCardHeader = ({
  title,
  isDataMissing,
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
      data-testid="governance-action-card-header"
    >
      <Typography
        sx={{
          fontSize: 18,
          fontWeight: 600,
          lineHeight: "24px",
          display: "-webkit-box",
          WebkitBoxOrient: "vertical",
          WebkitLineClamp: 2,
          wordBreak: "break-word",
          ...(isDataMissing && { color: "#9E2323" }),
        }}
      >
        {isDataMissing || title}
      </Typography>
      {isDataMissing && typeof isDataMissing === "string" && (
        <Tooltip
          heading={isDataMissing}
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
