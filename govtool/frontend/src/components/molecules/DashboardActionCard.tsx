import { Box, ButtonProps, Skeleton } from "@mui/material";
import { FC, ReactNode } from "react";

import { Button, CopyButton, Typography } from "@atoms";
import { useScreenDimension } from "@hooks";
import { theme } from "@/theme";

type DashboardActionCardProps = {
  description?: ReactNode;
  firstButtonAction?: () => void;
  firstButtonDisabled?: boolean;
  firstButtonLabel?: string;
  firstButtonVariant?: ButtonProps["variant"];
  imageHeight?: number;
  imageURL?: string;
  imageWidth?: number;
  secondButtonAction?: () => void;
  secondButtonLabel?: string;
  secondButtonVariant?: ButtonProps["variant"];
  title?: ReactNode;
  cardTitle?: string;
  cardId?: string;
  inProgress?: boolean;
  isLoading?: boolean;
  dataTestidFirstButton?: string;
  dataTestidSecondButton?: string;
  dataTestidDrepIdBox?: string;
  dataTestidDelegationStatus?: string;
};

export const DashboardActionCard: FC<DashboardActionCardProps> = ({
  ...props
}) => {
  const {
    dataTestidFirstButton,
    dataTestidSecondButton,
    dataTestidDrepIdBox,
    description,
    firstButtonAction,
    firstButtonDisabled = false,
    firstButtonLabel,
    firstButtonVariant = "contained",
    imageURL,
    secondButtonAction,
    secondButtonLabel,
    secondButtonVariant = "outlined",
    title,
    cardId,
    cardTitle,
    inProgress,
    isLoading = false,
  } = props;

  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile, screenWidth } = useScreenDimension();

  return (
    <Box
      mb={3}
      p={3}
      sx={{ boxShadow: `5px 5px 15px 5px ${boxShadow2}` }}
      borderRadius={3}
      flex={1}
      display="flex"
      flexDirection="column"
      border={inProgress && !isLoading ? 1 : 0}
      borderColor="accentOrange"
      position="relative"
      maxWidth={440}
    >
      {inProgress && !isLoading && (
        <Box
          bgcolor={"rgb(242, 217, 169)"}
          px={2.25}
          py={0.5}
          borderRadius={100}
          sx={{
            position: "absolute",
            top: -15,
            right: 30,
          }}
        >
          <Typography color={"orangeDark"} variant="body2">
            In progress
          </Typography>
        </Box>
      )}
      <Box display={"flex"} flexDirection="column" flex={1}>
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
            in progress
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
            borderColor={"rgba(236, 234, 234, 1)"}
            borderRadius={1.5}
            bgcolor={"white"}
            mb={3}
            sx={{ display: "flex", justifyContent: "flex-start" }}
          >
            <Box mr="24px">
              <Typography color={"#8E908E"} variant="caption">
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
          flexDirection={
            screenWidth < 768
              ? "column"
              : screenWidth < 1024
              ? "row"
              : screenWidth < 1440
              ? "column"
              : "row"
          }
        >
          {firstButtonLabel ? (
            <Button
              data-testid={dataTestidFirstButton}
              onClick={firstButtonAction}
              variant={firstButtonVariant}
              disabled={firstButtonDisabled}
              size="large"
              sx={{
                mr:
                  screenWidth < 768
                    ? 0
                    : screenWidth < 1024 && secondButtonLabel
                    ? 2
                    : screenWidth >= 1440 && secondButtonLabel
                    ? 2
                    : 0,
                width: isMobile ? "100%" : "auto",
              }}
            >
              {firstButtonLabel}
            </Button>
          ) : null}
          {secondButtonLabel ? (
            <Button
              data-testid={dataTestidSecondButton}
              onClick={secondButtonAction}
              variant={secondButtonVariant}
              size="large"
              sx={{
                width: isMobile ? "100%" : "auto",
                marginTop:
                  screenWidth < 768
                    ? 1
                    : screenWidth < 1024
                    ? 0
                    : screenWidth < 1440
                    ? 1
                    : 0,
              }}
            >
              {secondButtonLabel}
            </Button>
          ) : null}
        </Box>
      )}
    </Box>
  );
};
