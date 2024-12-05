import { Box, Link, Typography } from "@mui/material";
import { Trans, useTranslation } from "react-i18next";
import { useAppContext } from "@/context";
import { LINKS } from "@/consts/links";

export const TopBanners = () => {
  const { isMainnet, networkName, isInBootstrapPhase, isAppInitializing } =
    useAppContext();
  const { t } = useTranslation();

  if (isAppInitializing) {
    return null;
  }

  return (
    <Box position="relative">
      {/* NETWORK BANNER */}
      {!isMainnet && (
        <Box
          sx={{
            backgroundColor: "#BBE3F0",
            px: 2,
            py: 0.5,
            display: "flex",
            justifyContent: "center",
            gap: 1,
          }}
          data-testid="system-network-name"
        >
          <Typography variant="caption" fontWeight={600} color="primary">
            {`${t("network")}: ${networkName}`}
          </Typography>
          <Typography variant="caption" fontWeight={600} color="primary">
            |
          </Typography>
          <Link
            variant="caption"
            fontWeight={600}
            color="primary"
            href="https://gov.tools/"
          >
            {t("goToMainnet")}
          </Link>
        </Box>
      )}

      {/* BOOTSTRAPPING BANNER */}
      {isInBootstrapPhase && (
        <Box
          sx={{
            backgroundColor: "#1136A6",
            px: 2,
            py: 1,
            textAlign: "center",
          }}
          data-testid="system-bootstrapping-warning"
        >
          <Typography variant="caption" fontWeight={600} color="common.white">
            <Trans
              i18nKey="system.bootstrappingWarning"
              components={{
                docs: (
                  <Link
                    href={LINKS.BOOTSTRAPPING_PHASE}
                    target="_blank"
                    color="common.white"
                    data-testid="system-bootstrapping-warning-link"
                  />
                ),
              }}
            />
          </Typography>
        </Box>
      )}
    </Box>
  );
};
