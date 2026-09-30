import { Box } from "@mui/material";
import { Link } from "react-router";

import { Button } from "@atoms";
import { OUTCOMES_PATHS, OUTCOMES_TYPE_FILTERS } from "@consts";
import { useGetOutcomeGovActionMetadata, useTranslation } from "@hooks";
import { OutcomeGovernanceAction } from "@models";
import {
  getFullGovActionId,
  getOutcomeCIP129Id,
  getOutcomeProposalStatus,
} from "@utils";

import { GovernanceActionCardElement } from "./GovernanceActionCardElement";
import { GovernanceActionCardHeader } from "./GovernanceActionCardHeader";
import { OutcomeDatesBox } from "./OutcomeDatesBox";
import { OutcomeGovernanceActionStatus } from "./OutcomeGovernanceActionStatus";

type OutcomeCardProps = {
  action: OutcomeGovernanceAction;
};

export const OutcomeCard = ({ action }: OutcomeCardProps) => {
  const { metadata, metadataValid, isMetadataLoading } =
    useGetOutcomeGovActionMetadata(action);
  const { t } = useTranslation();

  const idCIP129 = getOutcomeCIP129Id(action);
  const fullGovActionId = getFullGovActionId(action.tx_hash, action.index);
  const isLive = getOutcomeProposalStatus(action.status) === "Live";

  const title = action.title || metadata?.data?.title;
  const abstract = action.abstract || metadata?.data?.abstract;
  const isDataMissing = metadata?.metadataStatus;
  const typeLabel =
    OUTCOMES_TYPE_FILTERS.find((filter) => filter.value === action.type)
      ?.label || action.type;

  return (
    <Box
      id={`${idCIP129}-outcome-card`}
      data-testid={`${idCIP129}-outcome-card`}
      sx={{
        width: "100%",
        height: "100%",
        display: "flex",
        flexDirection: "column",
        justifyContent: "space-between",
        boxShadow: "0px 4px 15px 0px #DDE3F5",
        borderRadius: "20px",
        backgroundColor: metadataValid
          ? "rgba(255, 255, 255, 0.3)"
          : "rgba(251, 235, 235, 0.50)",
        ...(!metadataValid && {
          border: isLive ? "1px solid #FFCBAD" : "1px solid #F6D5D5",
        }),
      }}
    >
      <Box
        data-testid={`${idCIP129}-outcome-card-content`}
        sx={{ padding: "24px 24px 0" }}
      >
        <GovernanceActionCardHeader
          title={title}
          isDataMissing={isDataMissing}
          isValidating={isMetadataLoading || (!isDataMissing && !title)}
          dataTestId={`${idCIP129}-card-title`}
        />
        <Box mb="20px">
          <OutcomeDatesBox action={action} isCard />
        </Box>
        {metadataValid && (
          <GovernanceActionCardElement
            label={t("outcome.abstract")}
            text={abstract ?? ""}
            textVariant="twoLines"
            dataTestId={`${idCIP129}-abstract`}
            isSliderCard
            isMarkdown
            isValidating={isMetadataLoading || !abstract}
          />
        )}
        <GovernanceActionCardElement
          label={t("outcome.governanceActionType")}
          text={typeLabel}
          textVariant="pill"
          dataTestId={`${idCIP129}-type`}
          isSliderCard
        />
        <Box mb="20px">
          <OutcomeGovernanceActionStatus
            status={action.status}
            actionId={idCIP129}
          />
        </Box>
        <GovernanceActionCardElement
          label={t("outcome.governanceActionId105")}
          text={fullGovActionId}
          dataTestId={`${fullGovActionId}-CIP-105-id`}
          isCopyButton
          isSliderCard
        />
        <GovernanceActionCardElement
          label={t("outcome.governanceActionId129")}
          text={idCIP129}
          dataTestId={`${idCIP129}-CIP-129-id`}
          isCopyButton
          isSliderCard
        />
      </Box>
      <Box
        data-testid={`${idCIP129}-outcome-card-actions`}
        sx={{
          boxShadow: "0px 4px 15px 0px #DDE3F5",
          borderBottomLeftRadius: 20,
          borderBottomRightRadius: 20,
          padding: 3,
          bgcolor: "white",
        }}
      >
        <Button
          component={Link}
          to={`${OUTCOMES_PATHS.governanceActionsOutcomes}/governance_actions/${fullGovActionId}`}
          size="large"
          sx={{ width: "100%" }}
          data-testid={`${fullGovActionId}-view-details`}
          aria-label={`${fullGovActionId}-view-details`}
        >
          {t("outcome.viewDetails")}
        </Button>
      </Box>
    </Box>
  );
};
