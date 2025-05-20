import { Box, Typography, IconButton } from "@mui/material";
import { useState } from "react";
import KeyboardArrowDownIcon from "@mui/icons-material/KeyboardArrowDown";
import KeyboardArrowUpIcon from "@mui/icons-material/KeyboardArrowUp";
import { Trans, useTranslation } from "react-i18next";

export const MaintenanceEndingBanner = () => {
  const [isExpanded, setIsExpanded] = useState(true);
  const { t } = useTranslation();

  const handleToggle = () => {
    setIsExpanded((prev) => !prev);
  };

  return (
    <Box
      sx={{
        backgroundColor: "#9c2224",
        width: "100%",
        overflow: "hidden",
        transition: "all 0.3s ease-in-out",
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
            onClick={handleToggle}
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
          maxHeight: isExpanded ? "500px" : "0px",
          transition: "max-height 0.3s ease-in-out",
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
            {t("system.maintenanceEnding.description3")}
          </Typography>
        </Box>
      </Box>
    </Box>
  );
};
