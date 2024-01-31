import { FC } from "react";
import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Button, Typography, Tooltip } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { theme } from "@/theme";
import {
  formatDisplayDate,
  getFullGovActionId,
  getProposalTypeLabel,
  getShortenedGovActionId,
} from "@utils";

interface ActionTypeProps
  extends Omit<
    ActionType,
    | "yesVotes"
    | "noVotes"
    | "abstainVotes"
    | "metadataHash"
    | "url"
    | "details"
    | "id"
    | "txHash"
    | "index"
  > {
  onClick?: () => void;
  inProgress?: boolean;
  txHash: string;
  index: number;
}

export const GovernanceActionCard: FC<ActionTypeProps> = ({ ...props }) => {
  const {
    type,
    inProgress = false,
    expiryDate,
    onClick,
    createdDate,
    txHash,
    index,
  } = props;
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  const {
    palette: { lightBlue },
  } = theme;

  const govActionId = getFullGovActionId(txHash, index);
  const proposalTypeNoEmptySpaces = getProposalTypeLabel(type).replace(
    / /g,
    ""
  );

  return (
    <Box
      maxWidth={402}
      border={inProgress ? 1 : 0}
      borderColor="lightOrange"
      minWidth={screenWidth < 375 ? 255 : screenWidth < 768 ? 294 : 402}
      sx={{
        boxShadow: inProgress
          ? "2px 2px 20px 0px #F55A0033"
          : "0px 4px 15px 0px #DDE3F5",
        borderRadius: "20px",
        backgroundColor: "transparent",
      }}
      position="relative"
      data-testid={`govaction-${proposalTypeNoEmptySpaces}-card`}
    >
      {inProgress && (
        <Box
          bgcolor="#F8ECD4"
          border={1}
          borderColor="#DEA029"
          px={2.25}
          py={0.5}
          borderRadius={100}
          sx={{
            position: "absolute",
            top: -15,
            right: 30,
          }}
        >
          <Typography color="#DEA029" variant="body2">
            {t("inProgress")}
          </Typography>
        </Box>
      )}
      <Box
        sx={{
          borderTopLeftRadius: 20,
          borderTopRightRadius: 20,
          backgroundColor: "rgba(255, 255, 255, 0.3)",
        }}
        pt={isMobile ? 3 : 5}
        px={isMobile ? 2.25 : 3}
        pb={3}
      >
        <Box data-testid="governance-action-type">
          <Typography color="neutralGray" variant="caption">
            {t("govActions.governanceActionType")}
          </Typography>
          <Box display={"flex"}>
            <Box
              mt={1.5}
              px={2.25}
              py={0.75}
              bgcolor={lightBlue}
              borderRadius={100}
            >
              <Typography
                data-testid={`${proposalTypeNoEmptySpaces}-type`}
                variant="caption"
              >
                {getProposalTypeLabel(type)}
              </Typography>
            </Box>
          </Box>
        </Box>
        <Box mt={5}>
          <Typography color="neutralGray" variant="caption">
            {t("govActions.governanceActionId")}
          </Typography>
          <Box display={"flex"} mt={0.25}>
            <Box
              px={2.25}
              py={0.75}
              border={1}
              borderColor={lightBlue}
              borderRadius={100}
            >
              <Typography
                data-testid={`${getFullGovActionId(txHash, index)}-id`}
                variant="caption"
              >
                {getShortenedGovActionId(txHash, index)}
              </Typography>
            </Box>
          </Box>
        </Box>
      </Box>
      {createdDate ? (
        <Box
          bgcolor="#D6E2FF80"
          display="flex"
          flexDirection="row"
          justifyContent="center"
          alignItems="center"
          py={0.75}
        >
          <Typography
            fontWeight={300}
            sx={{ flexWrap: "nowrap", mr: 1 }}
            variant="caption"
          >
            {t("govActions.submissionDate")}
          </Typography>
          <Typography
            fontWeight={600}
            sx={{ flexWrap: "nowrap" }}
            variant="caption"
          >
            {formatDisplayDate(createdDate)}
          </Typography>
          <Tooltip
            heading={t("tooltips.submissionDate.heading")}
            paragraphOne={t("tooltips.submissionDate.paragraphOne")}
            placement={"bottom-end"}
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
        </Box>
      ) : null}
      {expiryDate ? (
        <Box
          data-testid="expiry-date"
          bgcolor="rgba(247, 249, 251, 1)"
          display="flex"
          flexDirection="row"
          justifyContent="center"
          alignItems="center"
          py={0.75}
        >
          <Typography
            fontWeight={300}
            sx={{ flexWrap: "nowrap", mr: 1 }}
            variant="caption"
          >
            {t("govActions.expiryDate")}
          </Typography>
          <Typography
            fontWeight={600}
            sx={{ flexWrap: "nowrap" }}
            variant="caption"
          >
            {formatDisplayDate(expiryDate)}
          </Typography>
          <Tooltip
            heading={t("tooltips.expiryDate.heading")}
            paragraphOne={t("tooltips.expiryDate.paragraphOne")}
            paragraphTwo={t("tooltips.expiryDate.paragraphTwo")}
            placement={"bottom-end"}
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
        </Box>
      ) : null}
      <Box
        bgcolor="white"
        px={isMobile ? 1.5 : 3}
        py={2.5}
        sx={{ borderBottomLeftRadius: 20, borderBottomRightRadius: 20 }}
      >
        <Button
          onClick={onClick}
          variant={inProgress ? "outlined" : "contained"}
          size="large"
          sx={{
            width: "100%",
          }}
          data-testid={`govaction-${govActionId}-view-detail`}
        >
          {t(inProgress ? "seeTransaction" : "govActions.viewProposalDetails")}
        </Button>
      </Box>
    </Box>
  );
};
