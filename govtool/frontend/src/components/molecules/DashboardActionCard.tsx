import { Box, Skeleton } from "@mui/material";
import { FC, ReactNode } from "react";

import { LoadingButton, LoadingButtonProps, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { Card } from "./Card";

export type DashboardActionCardProps = {
  buttons?: LoadingButtonProps[];
  children?: ReactNode;
  dataTestidDelegationStatus?: string;
  description?: ReactNode;
  imageURL?: string;
  isLoading?: boolean;
  state?: "active" | "inProgress" | "default";
  title?: ReactNode;
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
    state = "default",
    title,
  } = props;

  const { screenWidth } = useScreenDimension();

  return (
    <Card
      {...(state === "inProgress" && {
        border: true,
        label: t("inProgress"),
        variant: "warning",
      })}
      sx={{
        flex: 1,
        display: "flex",
        flexDirection: "column",
        gap: 3,
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
          </Typography>
        ) : null}
        {state === "inProgress" && !isLoading ? (
          <Typography variant="title2" fontWeight={700}>
            {t("inProgress")}
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
      </Box>
      {children}
      <Box
        display="flex"
        flexDirection={screenWidth < 640 ? "column" : "row"}
        gap={{ xxs: 0, md: 2 }}
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
            <LoadingButton
              key={buttonProps.children?.toString()}
              data-testid={dataTestId}
              size="large"
              variant="outlined"
              sx={{
                width: {
                  xxs: "100%",
                  md: "auto",
                },
              }}
              {...buttonProps}
            />
          ))
        )}
      </Box>
    </Card>
  );
};
