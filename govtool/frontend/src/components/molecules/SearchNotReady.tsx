import { Typography } from "@mui/material";

import { useTranslation } from "@hooks";

/** In place of results while the backend cannot search everything yet. */
export const SearchNotReady = () => {
  const { t } = useTranslation();

  return (
    <Typography
      data-testid="search-not-ready"
      fontWeight={300}
      sx={{ py: 4 }}
    >
      {t("searchNotReady")}
    </Typography>
  );
};
