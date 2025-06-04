import { Box, Typography, IconButton, Link } from "@mui/material";
import KeyboardArrowDownIcon from "@mui/icons-material/KeyboardArrowDown";
import KeyboardArrowUpIcon from "@mui/icons-material/KeyboardArrowUp";
import { Trans, useTranslation } from "react-i18next";
import { useMaintenanceEndingBannerContext } from "./MaintenanceEndingBannerContext";

export const MaintenanceEndingBanner = () => {
  const { ref, isExpanded, toggleExpanded } =
    useMaintenanceEndingBannerContext();
  const { t } = useTranslation();

  return (
    <Box
      ref={ref}
      sx={{
        backgroundColor: isExpanded ? "#212A3D" : "#9c2224",
        width: "100%",
        overflow: "hidden",
        transition: "all 0.3s ease-in-out",
        position: "sticky",
        top: 0,
        zIndex: 1200,
      }}
    >
      {/* Banner Header */}
      <Box
        sx={{
          px: 2,
          py: 1,
          textAlign: "left",
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
        }}
      >
        <Box sx={{ display: "flex", alignItems: "center" }}>
          <Typography
            variant="body2"
            fontWeight={600}
            color="common.white"
            sx={{ display: "flex", alignItems: "center" }}
          >
            {t("system.maintenanceEnding.title")}
          </Typography>
        </Box>
        <Box sx={{ display: "flex" }}>
          <IconButton
            onClick={toggleExpanded}
            size="small"
            data-testid="toggle-maintenance-banner"
            sx={{
              color: "white",
              transition: "transform 0.3s ease",
            }}
          >
            {isExpanded ? <KeyboardArrowUpIcon /> : <KeyboardArrowDownIcon />}
          </IconButton>
        </Box>
      </Box>

      {/* Expandable Content */}
      <Box
        sx={{
          maxHeight: isExpanded ? '300px' : "0px", // 300px is a safe value for transition effect
          transition: "max-height 0.7s ease-in-out",
          overflow: "hidden",
        }}
      >
        <Box
          sx={{
            px: 2,
            pb: 2,
            color: "white",
            display: "flex",
            flexDirection: "column",
          }}
        >
          <Typography
            fontWeight={600}
            variant="caption"
            color="common.white"
            mb={0.5}
          >
            {t("system.maintenanceEnding.description1")}
          </Typography>
          <Typography variant="caption" color="common.white" mb={0.5}>
            <Trans
              i18nKey="system.maintenanceEnding.description2"
              components={[
                <Typography
                  key="1"
                  variant="caption"
                  fontWeight={600}
                  color="common.white"
                  component="span"
                />,
              ]}
            />
          </Typography>
          <Typography variant="caption" color="common.white">
            {t("system.maintenanceEnding.description3")}{" "}
            <Link
              variant="caption"
              fontWeight={600}
              color="common.white"
              href="https://docs.gov.tools/overview/important-updates/the-future-of-govtool-why-it-matters-and-what-comes-next"
              target="_blank"
              rel="noopener noreferrer"
              data-testid="govtool-future-link"
            >
              {t("system.maintenanceEnding.linkText")}
            </Link>
          </Typography>
        </Box>
      </Box>
    </Box>
  );
};
