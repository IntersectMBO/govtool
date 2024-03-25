import { Box, ButtonProps, Skeleton } from "@mui/material";
import { FC, ReactNode } from "react";

import { CopyButton, LoadingButton, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { theme } from "@/theme";

type DashboardActionCardProps = {
  cardId?: string;
  cardTitle?: string;
  dataTestidDelegationStatus?: string;
  dataTestidDrepIdBox?: string;
  dataTestidFirstButton?: string;
  dataTestidSecondButton?: string;
  description?: ReactNode;
  firstButtonAction?: () => void;
  firstButtonDisabled?: boolean;
  firstButtonIsLoading?: boolean;
  firstButtonLabel?: string;
  firstButtonVariant?: ButtonProps["variant"];
  imageURL?: string;
  inProgress?: boolean;
  isLoading?: boolean;
  secondButtonAction?: () => void;
  secondButtonIsLoading?: boolean;
  secondButtonLabel?: string;
  secondButtonVariant?: ButtonProps["variant"];
  title?: ReactNode;
};

export const DashboardActionCard: FC<DashboardActionCardProps> = ({
  ...props
}) => {
  const { t } = useTranslation();
  const {
    cardId,
    cardTitle,
    dataTestidDrepIdBox,
    dataTestidFirstButton,
    dataTestidSecondButton,
    description,
    firstButtonAction,
    firstButtonDisabled = false,
    firstButtonIsLoading = false,
    firstButtonLabel,
    firstButtonVariant = "contained",
    imageURL,
    inProgress,
    isLoading = false,
    secondButtonAction,
    secondButtonIsLoading = false,
    secondButtonLabel,
    secondButtonVariant = "outlined",
    title,
  } = props;

  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile, screenWidth } = useScreenDimension();

  return (
    <Box
      border={inProgress && !isLoading ? 1 : 0}
      borderColor="accentOrange"
      borderRadius={3}
      display="flex"
      flex={1}
      flexDirection="column"
      p={3}
      maxWidth={524}
      position="relative"
      sx={{ boxShadow: `5px 5px 15px 5px ${boxShadow2}` }}
    >
      {inProgress && !isLoading && (
        <Box
          bgcolor="rgb(242, 217, 169)"
          px={2.25}
          py={0.5}
          borderRadius={100}
          sx={{
            position: "absolute",
            top: -15,
            right: 30,
          }}
        >
          <Typography color="orangeDark" variant="body2">
            {t("inProgress")}
          </Typography>
        </Box>
      )}
      <Box display="flex" flexDirection="column" flex={1}>
        {imageURL ? (
          isLoading ? (
            <Skeleton
              animation="wave"
              variant="circular"
              width={50}
              height={50}
            />
          ) : (
            <img
              alt="card"
              src={imageURL}
              width={64}
              height={64}
              style={{ objectFit: "contain" }}
            />
          )
        ) : null}
        {title ? (
          <Typography sx={{ mt: 2 }} variant="title2">
            {isLoading ? <Skeleton variant="rounded" /> : title}
          </Typography>
        ) : null}
        {inProgress && !isLoading ? (
          <Typography variant="title2" fontWeight={700}>
            {t("inProgress")}
          </Typography>
        ) : null}
        {description ? (
          <Typography
            data-testid="voting-power-delegation-status"
            color="textGray"
            sx={{ mb: 3, mt: 1 }}
            variant="body2"
            fontWeight={400}
          >
            {isLoading ? (
              <Skeleton variant="rounded" height={60} />
            ) : (
              description
            )}
          </Typography>
        ) : null}
        {cardId && (
          <Box
            data-testid={dataTestidDrepIdBox}
            px={1.5}
            py={1}
            border={1}
            borderColor="rgba(236, 234, 234, 1)"
            borderRadius={1.5}
            bgcolor="white"
            mb={3}
            sx={{ display: "flex", justifyContent: "flex-start" }}
          >
            <Box mr="24px">
              <Typography color="#8E908E" variant="caption">
                {cardTitle}
              </Typography>
              <Typography
                variant="body2"
                sx={{
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  width:
                    screenWidth < 375
                      ? "150px"
                      : screenWidth < 425
                      ? "200px"
                      : screenWidth < 768
                      ? "240px"
                      : screenWidth < 1024
                      ? "300px"
                      : screenWidth < 1440
                      ? "150px"
                      : "300px",
                }}
              >
                {cardId}
              </Typography>
            </Box>
            <CopyButton text={cardId} />
          </Box>
        )}
      </Box>
      {isLoading ? (
        <Box display="flex" flexDirection="row">
          <Skeleton
            animation="wave"
            variant="rounded"
            width={100}
            height={35}
            sx={{ mr: 2 }}
          />
          <Skeleton
            animation="wave"
            variant="rounded"
            width={100}
            height={35}
          />
        </Box>
      ) : (
        <Box
          display="flex"
          flexDirection={screenWidth < 640 ? "column" : "row"}
        >
          {firstButtonLabel ? (
            <LoadingButton
              data-testid={dataTestidFirstButton}
              disabled={firstButtonDisabled}
              isLoading={firstButtonIsLoading}
              onClick={firstButtonAction}
              size="large"
              variant={firstButtonVariant}
              sx={{
                mr: screenWidth < 640 ? 0 : 2,
                width: isMobile ? "100%" : "auto",
              }}
            >
              {firstButtonLabel}
            </LoadingButton>
          ) : null}
          {secondButtonLabel ? (
            <LoadingButton
              data-testid={dataTestidSecondButton}
              isLoading={secondButtonIsLoading}
              onClick={secondButtonAction}
              size="large"
              variant={secondButtonVariant}
              sx={{
                width: isMobile ? "100%" : "auto",
                marginTop: screenWidth < 640 ? 1 : 0,
              }}
            >
              {secondButtonLabel}
            </LoadingButton>
          ) : null}
        </Box>
      )}
    </Box>
  );
};
