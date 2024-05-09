import { Box, Skeleton } from "@mui/material";
import { FC, ReactNode } from "react";

import { Button, LoadingButtonProps, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

import { Card } from "./Card";

export type DashboardActionCardProps = {
  buttons?: LoadingButtonProps[];
  children?: ReactNode;
  dataTestidDelegationStatus?: string;
  description?: ReactNode;
  imageURL?: string;
  isLoading?: boolean;
  isInProgressOnCard?: boolean;
  state?: "active" | "inProgress" | "default";
  title?: ReactNode;
  transactionId?: string | null;
  isSpaceBetweenButtons?: boolean;
};

export const DashboardActionCard: FC<DashboardActionCardProps> = ({
  ...props
}) => {
  const { t } = useTranslation();
  const {
    buttons,
    children,
    description,
    imageURL,
    isLoading = false,
    isInProgressOnCard = true,
    state = "default",
    title,
    transactionId,
    isSpaceBetweenButtons,
  } = props;

  const { screenWidth } = useScreenDimension();

  const onClickShowTransaction = () =>
    openInNewTab(`https://sancho.cexplorer.io/tx/${transactionId}`);

  return (
    <Card
      {...(state === "inProgress" && {
        border: true,
        label: t("inProgress"),
        variant: "warning",
      })}
      sx={{
        backgroundColor: state === "active" ? "#F0F4FF" : undefined,
        flex: 1,
        display: "flex",
        flexDirection: "column",
        maxWidth: 524,
      }}
    >
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
            {state === "inProgress" && !isLoading && isInProgressOnCard ? (
              <Typography
                variant="title2"
                fontWeight={600}
                sx={{ display: "inline" }}
              >
                {` ${t("inProgress")}`}
              </Typography>
            ) : null}
          </Typography>
        ) : null}
        {description ? (
          <Typography
            data-testid="voting-power-delegation-status"
            color="textGray"
            sx={{ mt: 1 }}
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
        {children}
        {transactionId && (
          <Button
            onClick={onClickShowTransaction}
            sx={{ width: "fit-content", p: 0 }}
            variant="text"
          >
            {t("dashboard.cards.showTransaction")}
          </Button>
        )}
      </Box>
      <Box
        display="flex"
        flexDirection={screenWidth < 640 ? "column" : "row"}
        justifyContent={isSpaceBetweenButtons ? "space-between" : undefined}
        mt={3}
        gap={2}
      >
        {isLoading ? (
          <>
            <Skeleton
              animation="wave"
              variant="rounded"
              width={100}
              height={35}
            />
            <Skeleton
              animation="wave"
              variant="rounded"
              width={100}
              height={35}
            />
          </>
        ) : (
          buttons?.map(({ dataTestId, ...buttonProps }) => (
            <Button
              key={buttonProps.children?.toString()}
              data-testid={dataTestId}
              size="large"
              variant="outlined"
              sx={{
                width: {
                  xxs: "100%",
                  md: "auto",
                },
                ...buttonProps.sx,
              }}
              {...buttonProps}
            />
          ))
        )}
      </Box>
    </Card>
  );
};
