import { NavLink, To } from "react-router-dom";
import { Box } from "@mui/material";
import Divider from "@mui/material/Divider";

import { useScreenDimension, useTranslation } from "@hooks";
import { Typography } from "@atoms";

type BreadcrumbsProps = {
  elementOne: string;
  elementOnePath: To;
  elementTwo: string;
  isDataMissing: boolean;
};

export const Breadcrumbs = ({
  elementOne,
  elementOnePath,
  elementTwo,
  isDataMissing,
}: BreadcrumbsProps) => {
  const { t } = useTranslation();
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
        {isDataMissing ? t("govActions.dataMissing") : elementTwo}
      </Typography>
    </Box>
  );
};
