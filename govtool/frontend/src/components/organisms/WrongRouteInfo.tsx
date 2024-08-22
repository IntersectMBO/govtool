import { Box } from "@mui/material";

import { Button, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";

import { useNavigate } from "react-router-dom";
import { PATHS } from "@/consts";

type Props = {
  title?: string;
  description?: string;
  primaryButtonText?: string;
  secondaryButtonText?: string;
  onPrimaryButton?: () => void;
  onSecondaryButton?: () => void;
  hideSecondaryButton?: boolean;
};

export const WrongRouteInfo = ({
  title,
  description,
  primaryButtonText,
  secondaryButtonText,
  hideSecondaryButton,
  onPrimaryButton,
  onSecondaryButton,
}: Props) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();

  const primaryButtonProps = {
    children: primaryButtonText ?? t("backToDashboard"),
    onClick: onPrimaryButton ?? (() => navigate(PATHS.dashboard)),
  };

  const secondaryButtonProps =
    primaryButtonText || onPrimaryButton
      ? {
          children: secondaryButtonText ?? t("backToDashboard"),
          onClick: onSecondaryButton ?? (() => navigate(PATHS.dashboard)),
        }
      : undefined;

  return (
    <>
      {title && (
        <Typography sx={{ textAlign: "center" }} variant="headline4">
          {title}
        </Typography>
      )}
      {description && (
        <Typography
          fontWeight={400}
          sx={{
            pb: isMobile ? 4 : 6,
            pt: 4,
            textAlign: "center",
            whiteSpace: "pre-line",
          }}
          variant="body1"
        >
          {description}
        </Typography>
      )}

      <Box
        sx={{
          display: "flex",
          flexDirection: isMobile ? "column-reverse" : "row",
          gap: 3,
          justifyContent: secondaryButtonProps ? "space-between" : "center",
        }}
      >
        {!hideSecondaryButton && secondaryButtonProps && (
          <Button
            data-testid="secondary-button"
            size="extraLarge"
            sx={{
              px: 6,
            }}
            variant="outlined"
            {...secondaryButtonProps}
          />
        )}
        <Button
          data-testid="primary-button"
          size="extraLarge"
          sx={{
            px: 6,
          }}
          variant="contained"
          {...primaryButtonProps}
        />
      </Box>
    </>
  );
};
