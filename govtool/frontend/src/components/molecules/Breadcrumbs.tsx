import { NavLink, To } from "react-router-dom";
import { Box } from "@mui/material";
import Divider from "@mui/material/Divider";

import { useScreenDimension } from "@hooks";
import { Typography } from "@atoms";
import { getMetadataDataMissingStatusTranslation } from "@/utils";
import { MetadataValidationStatus } from "@/models";

type BreadcrumbsProps = {
  elementOne: string;
  elementOnePath: To;
  elementTwo: string;
  isDataMissing: MetadataValidationStatus | null;
};

export const Breadcrumbs = ({
  elementOne,
  elementOnePath,
  elementTwo,
  isDataMissing,
}: BreadcrumbsProps) => {
  const { isMobile } = useScreenDimension();
  return (
    <Box
      sx={{
        display: "flex",
        alignItems: "center",
        margin: `2px 0 ${isMobile ? "44px" : "24px"}`,
      }}
    >
      <NavLink to={elementOnePath} style={{ textDecorationColor: "#0033AD" }}>
        <Typography
          color="primary"
          variant="caption"
          sx={{
            whiteSpace: "nowrap",
          }}
        >
          {elementOne}
        </Typography>
      </NavLink>
      <Divider
        orientation="vertical"
        flexItem
        color="textBlack"
        sx={{ margin: "0 6px" }}
      />
      <Typography
        variant="caption"
        sx={{
          fontWeight: 500,
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
    </Box>
  );
};
