import { Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";

import { useScreenDimension, useTranslation } from "@hooks";
import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { theme } from "@/theme";

type Props = {
  filtersOpen?: boolean;
  setFiltersOpen?: Dispatch<SetStateAction<boolean>>;
  chosenFiltersLength?: number;
  sortOpen: boolean;
  setSortOpen: Dispatch<SetStateAction<boolean>>;
  sortingActive: boolean;
  isFiltering?: boolean;
};

export const OrderActionsChip = (props: Props) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

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
    <Box
      display="flex"
      width="min-content"
      alignItems="center"
      ml="8px"
      gap={isMobile ? "8px" : "24px"}
    >
      {isFiltering && (
        <Box
          sx={{
            position: "relative",
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            cursor: "pointer",
            ...(!isMobile && {
              background: filtersOpen ? secondary.main : "transparent",
              borderRadius: "99px",
              padding: "12px 14px",
            }),
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
              borderRadius: "100%",
              marginRight: "8px",
              overflow: "visible",
              height: 20,
              width: 20,
              objectFit: "contain",
              ...(isMobile && {
                background: filtersOpen ? secondary.main : "transparent",
                padding: "14px",
                marginRight: "0",
              }),
            }}
          />
          {!isMobile && (
            <Typography
              variant="body1"
              sx={{
                color: filtersOpen ? "white" : "primaryBlue",
                fontWeight: 500,
              }}
            >
              {t("filter")}
            </Typography>
          )}
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
                right: "-3px",
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
          ...(!isMobile && {
            background: sortOpen ? secondary.main : "transparent",
            borderRadius: "99px",
            padding: "12px 14px",
          }),
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
            borderRadius: "100%",
            marginRight: "8px",
            height: 20,
            width: 20,
            objectFit: "contain",
            ...(isMobile && {
              background: sortOpen ? secondary.main : "transparent",
              padding: "14px",
              marginRight: "0",
            }),
          }}
        />
        {!isMobile && (
          <Typography
            variant="body1"
            sx={{
              color: sortOpen ? "white" : "primaryBlue",
              fontWeight: 500,
            }}
          >
            {t("sort")}
          </Typography>
        )}
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
              right: "-3px",
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
