import { useTranslation } from "@hooks";

import { Card } from "./Card";
import { Typography } from "../atoms";

export const EmptyStateDrepDirectory = () => {
  const { t } = useTranslation();

  return (
    <Card
      border
      elevation={0}
      sx={{
        alignItems: "center",
        display: "flex",
        flexDirection: "column",
        gap: 1,
        py: 5,
        width: "-webkit-fill-available",
      }}
    >
      <Typography fontSize={22}>
        {t("dRepDirectory.noResultsForTheSearchTitle")}
      </Typography>
      <Typography fontWeight={400}>
        {t("dRepDirectory.noResultsForTheSearchDescription")}
      </Typography>
    </Card>
  );
};
