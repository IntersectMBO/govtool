import { useState } from "react";
import { Box } from "@mui/material";

import { Button, Typography } from "@atoms";
import { useCountdown, useTranslation } from "@hooks";
import type { MetadataAnchor, MetadataRefreshOutcome } from "@models";
import { useMetadataRetryMutation } from "@/hooks/mutations";

type MetadataRetryButtonProps = {
  anchor: MetadataAnchor;
  onOutcome?: (outcome: MetadataRefreshOutcome) => void;
};

/**
 * Retry past a cached failure (D118, D125). The limit is per anchor, whoever
 * clicks: inside the window the backend fetches nothing and answers with the
 * seconds left, which count down here before the button re-enables.
 */
export const MetadataRetryButton = ({
  anchor,
  onOutcome,
}: MetadataRetryButtonProps) => {
  const { t } = useTranslation();
  const { retry, isRetrying } = useMetadataRetryMutation(anchor);
  const { remaining, isRunning, start } = useCountdown();
  const [isError, setIsError] = useState(false);

  const onClick = async () => {
    setIsError(false);
    try {
      const outcome = await retry();
      if (outcome.retryAfterSeconds && outcome.retryAfterSeconds > 0) {
        start(outcome.retryAfterSeconds);
      }
      onOutcome?.(outcome);
    } catch {
      setIsError(true);
    }
  };

  return (
    <Box sx={{ display: "flex", alignItems: "center", gap: 2, flexWrap: "wrap" }}>
      <Button
        data-testid="metadata-retry-button"
        size="medium"
        variant="outlined"
        disabled={isRunning}
        isLoading={isRetrying}
        onClick={onClick}
      >
        {t("metadataReport.retry.button")}
      </Button>
      {isRunning && (
        <Typography
          variant="body2"
          data-testid="metadata-retry-countdown"
          sx={{ color: "neutralGray" }}
        >
          {t("metadataReport.retry.wait", { count: remaining })}
        </Typography>
      )}
      {isError && (
        <Typography variant="body2" sx={{ color: "errorRed" }}>
          {t("metadataReport.retry.error")}
        </Typography>
      )}
    </Box>
  );
};
