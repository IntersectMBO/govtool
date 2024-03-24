import { Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";

import { useTranslation } from "@hooks";
import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { theme } from "@/theme";

interface Props {
  filtersOpen?: boolean;
  setFiltersOpen?: Dispatch<SetStateAction<boolean>>;
  chosenFiltersLength?: number;
  sortOpen: boolean;
  setSortOpen: Dispatch<SetStateAction<boolean>>;
  sortingActive: boolean;
  isFiltering?: boolean;
}

export const OrderActionsChip = (props: Props) => {
  const { t } = useTranslation();

  const {
    palette: { secondary },
  } = theme;

  const {
    filtersOpen,
    setFiltersOpen = () => {},
    chosenFiltersLength = 0,
    sortOpen,
    setSortOpen,
    sortingActive,
    isFiltering = true,
  } = props;

  return (
    <Box display="flex" width="min-content" alignItems="center" ml="8px">
      {isFiltering && (
        <Box
          sx={{
            position: "relative",
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            cursor: "pointer",
          }}
          onClick={() => {
            setSortOpen(false);
            if (isFiltering) {
              setFiltersOpen(!filtersOpen);
            }
          }}
          data-testid="filters-button"
        >
          <img
            alt="filter"
            src={filtersOpen ? ICONS.filterWhiteIcon : ICONS.filterIcon}
            style={{
              background: filtersOpen ? secondary.main : "transparent",
              borderRadius: "100%",
              padding: "14px",
              overflow: "visible",
              height: 20,
              width: 20,
              objectFit: "contain",
            }}
          />
          <Typography
            variant="body1"
            sx={{
              color: "primaryBlue",
              fontWeight: 500,
            }}
          >
            {t("filter")}
          </Typography>
          {!filtersOpen && chosenFiltersLength > 0 && (
            <Box
              sx={{
                alignItems: "center",
                background: secondary.main,
                borderRadius: "100%",
                color: "white",
                display: "flex",
                fontSize: "12px",
                height: "16px",
                justifyContent: "center",
                position: "absolute",
                left: "32px",
                top: "0",
                width: "16px",
              }}
            >
              <Typography variant="caption" color="#FFFFFF">
                {chosenFiltersLength}
              </Typography>
            </Box>
          )}
        </Box>
      )}
      <Box
        sx={{
          position: "relative",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          cursor: "pointer",
        }}
        onClick={() => {
          if (isFiltering) {
            setFiltersOpen(false);
          }
          setSortOpen(!sortOpen);
        }}
        data-testid="sort-button"
      >
        <img
          alt="sort"
          src={sortOpen ? ICONS.sortWhiteIcon : ICONS.sortIcon}
          style={{
            background: sortOpen ? secondary.main : "transparent",
            borderRadius: "100%",
            padding: "14px",
            height: 24,
            width: 24,
            objectFit: "contain",
          }}
        />
        <Typography
          variant="body1"
          sx={{
            color: "primaryBlue",
            fontWeight: 500,
          }}
        >
          {t("sort")}
        </Typography>
        {!sortOpen && sortingActive && (
          <Box
            sx={{
              alignItems: "center",
              background: secondary.main,
              borderRadius: "100%",
              color: "white",
              display: "flex",
              fontSize: "12px",
              height: "16px",
              justifyContent: "center",
              position: "absolute",
              left: "36px",
              top: "0",
              width: "16px",
            }}
          >
            <img alt="sorting active" src={ICONS.sortActiveIcon} />
          </Box>
        )}
      </Box>
    </Box>
  );
};
