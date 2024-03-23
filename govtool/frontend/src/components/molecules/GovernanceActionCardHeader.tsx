import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Tooltip, Typography } from "@atoms";
import { useTranslation } from "@hooks";

const mockedLongText =
  "Lorem ipsum dolor sit, amet consectetur adipisicing elit. Sit, distinctio culpa minus eaque illo quidem voluptates quisquam mollitia consequuntur ex, sequi saepe? Ad ex adipisci molestiae sed.";

type GovernanceActionCardHeaderProps = {
  title: string;
  isDataMissing: boolean;
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
      }}
    >
      <Typography
        sx={{
          fontSize: 22,
          fontWeight: 400,
          lineHeight: "28px",
          overflow: "hidden",
          textOverflow: "ellipsis",
          display: "-webkit-box",
          WebkitBoxOrient: "vertical",
          WebkitLineClamp: 2,
          ...(isDataMissing && { color: "#9E2323" }),
        }}
      >
        {isDataMissing ? t("govActions.dataMissing") : title}
      </Typography>
      {isDataMissing && (
        <Tooltip
          heading="Data Missing"
          paragraphOne={mockedLongText}
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
