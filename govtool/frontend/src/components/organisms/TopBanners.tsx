import { Box, Link, Typography } from "@mui/material";
import { Trans } from "react-i18next";
import { useAppContext } from "@/context";

export const TopBanners = () => {
  const { isMainnet, networkName, isInBootstrapPhase } = useAppContext();

  return (
    <Box position="relative">
      {/* NETWORK BANNER */}
      {!isMainnet && (
        <Box
          sx={{
            backgroundColor: "#BBE3F0",
            px: 2,
            py: 0.5,
            textAlign: "center",
          }}
        >
          <Typography variant="caption" fontWeight={600} color="primary">
            {networkName}
          </Typography>
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
        >
          <Typography variant="caption" fontWeight={600} color="common.white">
            <Trans
              i18nKey="system.bootstrappingWarning"
              components={{
                docs: (
                  <Link
                    href="https://docs.gov.tools/about/bootstrapping-phase"
                    target="_blank"
                    color="common.white"
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
