import { Box, Divider } from "@mui/material";

import { Button, Typography } from "@atoms";
import { primaryBlue } from "@consts";
import { useAppContext, useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab, testIdFromLabel } from "@utils";

import { Card } from "./Card";
import { AutomatedVotingCardProps } from "./types";

export const AutomatedVotingCard = ({
  dataTestId,
  description,
  inProgress,
  isConnected,
  isDelegateLoading,
  isSelected,
  onClickDelegate,
  onClickInfo,
  title,
  transactionId,
  votingPower,
}: AutomatedVotingCardProps) => {
  const { cExplorerBaseUrl } = useAppContext();
  const { isMobile, screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const { t } = useTranslation();
  const testIdLabel = testIdFromLabel(title);

  const onClickShowTransaction = () =>
    openInNewTab(`${cExplorerBaseUrl}/tx/${transactionId}`);

  return (
    <Card
      {...(inProgress && {
        variant: "warning",
        label: t("inProgress"),
      })}
      {...(isSelected && {
        variant: "primary",
      })}
      sx={{
        alignItems: "center",
        bgcolor: (theme) => `${theme.palette.neutralWhite}40`,
        boxShadow: `0px 4px 15px 0px ${primaryBlue.c100}`,
        display: "flex",
        flex: 1,
        flexDirection: screenWidth < 1024 ? "column" : "row",
        justifyContent: "space-between",
        mt: inProgress || isSelected ? 2 : 0,
        py: 2.25,
      }}
      dataTestId={dataTestId ?? `${testIdLabel}-card`}
    >
      <Box
        sx={{
          flex: 1,
          mb: screenWidth < 1024 ? 1.5 : 0,
          width: screenWidth < 1024 ? "100%" : "auto",
        }}
      >
        <Typography>{title}</Typography>
        <Typography
          fontWeight={400}
          sx={{
            mt: 0.5,
            color: screenWidth < 1024 ? "neutralGray" : "textBlack",
            width: screenWidth < 1024 ? "auto" : "60%",
          }}
          variant="body2"
        >
          {description}
        </Typography>
        {transactionId && (
          <Button
            onClick={onClickShowTransaction}
            sx={{ width: "fit-content", p: 0 }}
            variant="text"
          >
            {t("seeTransaction")}
          </Button>
        )}
      </Box>
      {!inProgress && !isSelected && (
        <>
          <Divider
            flexItem
            orientation={screenWidth < 1024 ? "horizontal" : "vertical"}
            sx={{ ml: screenWidth < 1024 ? 0 : 1 }}
            variant={screenWidth < 1024 ? "fullWidth" : "middle"}
          />
          <Box
            sx={{
              alignContent: "flex-start",
              display: "flex",
              flexDirection: "column",
              px: screenWidth < 1024 ? 0 : 4.25,
              py: screenWidth < 1024 ? 1 : 0,
              width: screenWidth < 1024 ? "100%" : "auto",
            }}
          >
            <Typography color="neutralGray" fontWeight={500} variant="caption">
              {t("dRepDirectory.votingPower")}
            </Typography>
            <Typography sx={{ display: "flex", flexDirection: "row", mt: 0.5 }}>
              {"₳ "}
              {votingPower}
            </Typography>
          </Box>
          <Divider
            flexItem
            orientation={screenWidth < 1024 ? "horizontal" : "vertical"}
            sx={{ mr: screenWidth < 1024 ? 0 : 1 }}
            variant={screenWidth < 1024 ? "fullWidth" : "middle"}
          />
          <Box
            sx={{
              display: "flex",
              flexDirection: "row",
              gap: 2.5,
              mt: screenWidth < 1024 ? 3 : 0,
              width: screenWidth < 1024 ? "100%" : "auto",
            }}
          >
            <Button
              data-testid={`${testIdLabel}-info-button`}
              onClick={onClickInfo}
              size={isMobile ? "medium" : "large"}
              sx={{ flex: screenWidth < 768 ? 1 : undefined }}
              variant="outlined"
            >
              {t("info")}
            </Button>
            {!isConnected ? (
              <Button
                data-testid={`${testIdLabel}-connect-to-delegate-button`}
                onClick={() => openModal({ type: "chooseWallet" })}
                size={isMobile ? "medium" : "large"}
                sx={{ flex: screenWidth < 768 ? 1 : undefined }}
              >
                {t("connectToDelegate")}
              </Button>
            ) : (
              !isSelected && (
                <Button
                  data-testid={`${testIdLabel}-delegate-button`}
                  isLoading={isDelegateLoading}
                  onClick={onClickDelegate}
                  size={isMobile ? "medium" : "large"}
                  sx={{ flex: screenWidth < 768 ? 1 : undefined }}
                  variant="contained"
                >
                  {t("delegate")}
                </Button>
              )
            )}
          </Box>
        </>
      )}
    </Card>
  );
};
