import { NavLink, To } from "react-router";
import { Box, Skeleton } from "@mui/material";
import { ChevronRight } from "lucide-react";

import { useScreenDimension } from "@hooks";
import { Typography } from "@atoms";
import { getMetadataDataMissingStatusTranslation } from "@/utils";
import { MetadataValidationStatus } from "@/models";
import { ds } from "@/consts/designTokens";

type BreadcrumbsProps = {
  elementOne: string;
  elementOnePath: To;
  elementTwo: string;
  isDataMissing: MetadataValidationStatus | null;
  isMetadataLoading?: boolean;
};

export const Breadcrumbs = ({
  elementOne,
  elementOnePath,
  elementTwo,
  isDataMissing,
  isMetadataLoading,
}: BreadcrumbsProps) => {
  const { isMobile } = useScreenDimension();
  return (
    <Box
      data-testid="breadcrumb-component"
      sx={{
        display: "flex",
        alignItems: "center",
        margin: `2px 0 ${isMobile ? "44px" : "24px"}`,
      }}
    >
      <NavLink to={elementOnePath} style={{ textDecoration: "none" }}>
        <Typography
          color={ds.gray["500"]}
          variant="tagline"
          sx={{
            whiteSpace: "nowrap",
            "&:hover": { color: ds.gray["900"] },
          }}
        >
          {elementOne}
        </Typography>
      </NavLink>
      <Box
        component="span"
        sx={{ color: ds.gray["400"], display: "inline-flex", mx: 1 }}
      >
        <ChevronRight aria-hidden size={12} />
      </Box>
      {isMetadataLoading ? (
        <Skeleton variant="rounded" width={200} height={15} />
      ) : (
        <Typography
          color={ds.gray["900"]}
          variant="tagline"
          fontWeight={600}
          sx={{
            whiteSpace: "nowrap",
            overflow: "hidden",
            textOverflow: "ellipsis",
          }}
        >
          {(isDataMissing &&
            getMetadataDataMissingStatusTranslation(
              isDataMissing as MetadataValidationStatus,
            )) ||
            elementTwo}
        </Typography>
      )}
    </Box>
  );
};
