import { Box } from "@mui/material";
import { Link } from "react-router";

import { Button } from "@atoms";
import { GOV_ACTION_HISTORY_PATHS, GOV_ACTION_HISTORY_TYPE_FILTERS } from "@consts";
import { useGetGovernanceActionMetadata, useTranslation } from "@hooks";
import { GovernanceActionRecord } from "@models";
import {
  getFullGovActionId,
  getGovernanceActionCIP129Id,
  getGovernanceActionProposalStatus,
} from "@utils";

import { GovernanceActionCardElement } from "./GovernanceActionCardElement";
import { GovernanceActionCardHeader } from "./GovernanceActionCardHeader";
import { GovernanceActionDatesBox } from "./GovernanceActionDatesBox";
import { GovernanceActionStatus } from "./GovernanceActionStatus";

type GovernanceActionHistoryCardProps = {
  action: GovernanceActionRecord;
};

export const GovernanceActionHistoryCard = ({ action }: GovernanceActionHistoryCardProps) => {
  const { metadata, metadataValid, isMetadataLoading } =
    useGetGovernanceActionMetadata(action);
  const { t } = useTranslation();

  const idCIP129 = getGovernanceActionCIP129Id(action);
  const fullGovActionId = getFullGovActionId(action.tx_hash, action.index);
  const isLive = getGovernanceActionProposalStatus(action.status) === "Live";

  const title = action.title || metadata?.data?.title;
  const abstract = action.abstract || metadata?.data?.abstract;
  const isDataMissing = metadata?.metadataStatus;
  const typeLabel =
    GOV_ACTION_HISTORY_TYPE_FILTERS.find((filter) => filter.value === action.type)
      ?.label || action.type;

  return (
    <Box
      id={`${idCIP129}-governance-action-card`}
      data-testid={`${idCIP129}-governance-action-card`}
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
        data-testid={`${idCIP129}-governance-action-card-content`}
        sx={{ padding: "24px 24px 0" }}
      >
        <GovernanceActionCardHeader
          title={title}
          isDataMissing={isDataMissing}
          isValidating={isMetadataLoading || (!isDataMissing && !title)}
          dataTestId={`${idCIP129}-card-title`}
        />
        <Box mb="20px">
          <GovernanceActionDatesBox action={action} isCard />
        </Box>
        {metadataValid && (
          <GovernanceActionCardElement
            label={t("actionRecord.abstract")}
            text={abstract ?? ""}
            textVariant="twoLines"
            dataTestId={`${idCIP129}-abstract`}
            isSliderCard
            isMarkdown
            isValidating={isMetadataLoading || !abstract}
          />
        )}
        <GovernanceActionCardElement
          label={t("actionRecord.governanceActionType")}
          text={typeLabel}
          textVariant="pill"
          dataTestId={`${idCIP129}-type`}
          isSliderCard
        />
        <Box mb="20px">
          <GovernanceActionStatus
            status={action.status}
            actionId={idCIP129}
          />
        </Box>
        <GovernanceActionCardElement
          label={t("actionRecord.governanceActionId105")}
          text={fullGovActionId}
          dataTestId={`${fullGovActionId}-CIP-105-id`}
          isCopyButton
          isSliderCard
        />
        <GovernanceActionCardElement
          label={t("actionRecord.governanceActionId129")}
          text={idCIP129}
          dataTestId={`${idCIP129}-CIP-129-id`}
          isCopyButton
          isSliderCard
        />
      </Box>
      <Box
        data-testid={`${idCIP129}-governance-action-card-actions`}
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
          to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${fullGovActionId}`}
          size="large"
          sx={{ width: "100%" }}
          data-testid={`${fullGovActionId}-view-details`}
          aria-label={`${fullGovActionId}-view-details`}
        >
          {t("actionRecord.viewDetails")}
        </Button>
      </Box>
    </Box>
  );
};
