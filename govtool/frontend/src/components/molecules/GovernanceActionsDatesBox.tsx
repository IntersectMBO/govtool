import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";
import { Trans } from "react-i18next";

import { Tooltip, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { formatDisplayDate } from "@/utils";

type GovernanceActionsDatesBoxProps = {
  createdDate: string;
  expiryDate: string | undefined;
  expiryEpochNo: number | undefined;
  createdEpochNo: number;
  isSliderCard?: boolean;
};

export const GovernanceActionsDatesBox = ({
  createdDate,
  expiryDate,
  expiryEpochNo,
  createdEpochNo,
  isSliderCard,
}: GovernanceActionsDatesBoxProps) => {
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();

  const isFontSizeSmaller = screenWidth < 420;

  return (
    <Box
      sx={{
        border: 1,
        borderColor: "lightBlue",
        borderRadius: 3,
        display: "flex",
        flexDirection: "column",
        overflow: "hidden",
        mb: isSliderCard ? "20px" : "32px",
      }}
    >
      <Box
        sx={{
          alignItems: "center",
          bgcolor: "#D6E2FF80",
          display: "flex",
          flex: 1,
          justifyContent: "center",
          py: "6px",
          width: "100%",
        }}
      >
        <Typography
          variant="caption"
          sx={{
            fontSize: isFontSizeSmaller ? 11 : 12,
            fontWeight: 300,
          }}
          data-testid="submission-date"
        >
          <Trans
            i18nKey="govActions.submittedDateWithEpoch"
            values={{
              date: formatDisplayDate(createdDate),
              epoch: createdEpochNo,
            }}
            components={[
              <span style={{ fontWeight: 600 }} key="0" />,
              <span style={{ fontWeight: 400 }} key="1" />,
            ]}
          />
        </Typography>
        <Tooltip
          heading={t("tooltips.submissionDate.heading")}
          paragraphOne={t("tooltips.submissionDate.paragraphOne")}
          placement="bottom-end"
          arrow
        >
          <InfoOutlinedIcon
            sx={{
              ml: 0.5,
              fontSize: isFontSizeSmaller ? "18px" : "19px",
              color: "#ADAEAD",
            }}
          />
        </Tooltip>
      </Box>
      <Box
        sx={{
          justifyContent: "center",
          alignItems: "center",
          display: "flex",
          flex: 1,
          py: 0.75,
          width: "100%",
        }}
      >
        <Typography
          variant="caption"
          sx={{
            fontSize: isFontSizeSmaller ? 11 : 12,
            fontWeight: 300,
          }}
          data-testid="expiry-date"
        >
          <Trans
            i18nKey="govActions.expiresDateWithEpoch"
            values={{
              date: expiryDate ? formatDisplayDate(expiryDate) : "-",
              epoch: expiryEpochNo ?? "-",
            }}
            components={[
              <span style={{ fontWeight: 600 }} key="0" />,
              <span style={{ fontWeight: 400 }} key="1" />,
            ]}
          />
        </Typography>
        <Tooltip
          heading={t("tooltips.expiryDate.heading")}
          paragraphOne={t("tooltips.expiryDate.paragraphOne")}
          paragraphTwo={t("tooltips.expiryDate.paragraphTwo")}
          placement="bottom-end"
          arrow
        >
          <InfoOutlinedIcon
            sx={{
              ml: 0.5,
              fontSize: isFontSizeSmaller ? "18px" : "19px",
              color: "#ADAEAD",
            }}
          />
        </Tooltip>
      </Box>
    </Box>
  );
};
