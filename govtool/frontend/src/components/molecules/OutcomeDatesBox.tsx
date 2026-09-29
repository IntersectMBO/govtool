import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Tooltip, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { OutcomeGovernanceAction } from "@models";
import {
  formatOutcomeTimestamp,
  getOutcomeCIP129Id,
  getOutcomeProposalStatus,
} from "@utils";

type OutcomeDatesBoxProps = {
  action: OutcomeGovernanceAction;
  isCard?: boolean;
};

const InfoIcon = () => (
  <InfoOutlinedIcon sx={{ fontSize: "19px", color: "#ADAEAD" }} />
);

/**
 * Submission date, then the date that ended the action (expired, not
 * ratified, enacted) or, while it can still pass, its expiry.
 */
export const OutcomeDatesBox = ({
  action,
  isCard = false,
}: OutcomeDatesBoxProps) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const dateFormat = isCard || isMobile ? "short" : "full";
  const idCIP129 = getOutcomeCIP129Id(action);

  const lastDate = (() => {
    switch (getOutcomeProposalStatus(action.status)) {
      case "Expired":
        return {
          label: t("outcome.dates.expired.label"),
          date: action.status_times.expired_time,
          epoch: action.status.expired_epoch,
          tooltip: "expired",
        };
      case "Not Ratified":
        return {
          label: t("outcome.dates.notRatified.label"),
          date: action.status_times.dropped_time,
          epoch: action.status.dropped_epoch,
          tooltip: "notRatified",
        };
      case "Enacted":
        return {
          label: t("outcome.dates.enacted.label"),
          date: action.status_times.enacted_time,
          epoch: action.status.enacted_epoch,
          tooltip: "enacted",
        };
      default:
        return {
          label: t("outcome.dates.expires"),
          date: action.expiry_date,
          epoch: action.expiration,
          tooltip: "expiry",
        };
    }
  })();

  const rows = [
    {
      testId: `${idCIP129}-submitted-date`,
      label: t("outcome.dates.submitted"),
      date: action.time,
      epoch: action.epoch_no,
      tooltipHeading: t("outcome.dates.submission.title"),
      tooltipParagraphOne: t("outcome.dates.submission.description"),
      tooltipParagraphTwo: undefined,
    },
    {
      // e.g. `<id>-Expired-date`, which the playwright suite reads.
      testId: `${idCIP129}-${lastDate.label.replace(":", "")}-date`,
      label: lastDate.label,
      date: lastDate.date,
      epoch: lastDate.epoch,
      tooltipHeading: t(`outcome.dates.${lastDate.tooltip}.title`),
      tooltipParagraphOne: t(`outcome.dates.${lastDate.tooltip}.paragraphOne`),
      tooltipParagraphTwo:
        lastDate.tooltip === "expiry"
          ? t("outcome.dates.expiry.paragraphTwo")
          : undefined,
    },
  ];

  return (
    <Box
      data-testid={`${idCIP129}-dates`}
      sx={{
        border: 1,
        borderColor: "lightBlue",
        borderRadius: 3,
        display: "flex",
        flexDirection: "column",
        overflow: "hidden",
        textAlign: "center",
      }}
    >
      {rows.map((row, index) => (
        <Box
          key={row.testId}
          data-testid={row.testId}
          sx={{
            alignItems: "center",
            bgcolor: index === 0 ? "#D6E2FF80" : undefined,
            display: "flex",
            flexWrap: "wrap",
            gap: 0.5,
            justifyContent: "center",
            py: "6px",
          }}
        >
          <Typography variant="caption" sx={{ fontSize: 12, fontWeight: 300 }}>
            {row.label}{" "}
            <Typography
              component="span"
              variant="caption"
              sx={{ fontSize: 12, fontWeight: 600 }}
            >
              {row.date ? formatOutcomeTimestamp(row.date, dateFormat) : "-"}
            </Typography>
          </Typography>
          <Box display="flex" alignItems="center" gap={0.5}>
            <Typography variant="caption" sx={{ fontSize: 12 }}>
              ({t("outcome.epoch")} {row.epoch ?? "-"})
            </Typography>
            <Tooltip
              heading={row.tooltipHeading}
              paragraphOne={row.tooltipParagraphOne}
              paragraphTwo={row.tooltipParagraphTwo}
              placement="bottom-end"
              arrow
            >
              <Box display="flex">
                <InfoIcon />
              </Box>
            </Tooltip>
          </Box>
        </Box>
      ))}
    </Box>
  );
};
