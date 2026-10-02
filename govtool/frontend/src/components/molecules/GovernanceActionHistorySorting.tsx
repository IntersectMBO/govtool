import { MouseEvent, useState } from "react";
import {
  Box,
  Divider,
  Fade,
  FormControl,
  FormControlLabel,
  Menu,
  Radio,
  RadioGroup,
} from "@mui/material";
import { useSearchParams } from "react-router";

import { Typography } from "@atoms";
import { fadedPurple, GOV_ACTION_HISTORY_SORT_OPTIONS } from "@consts";
import { useTranslation } from "@hooks";
import { getCurrentSearchParams } from "@utils";

import { GovernanceActionHistoryMenuButton } from "./GovernanceActionHistoryMenuButton";

/** The sort order, kept in the `sort` URL parameter. */
export const GovernanceActionHistorySorting = () => {
  const [searchParams, setSearchParams] = useSearchParams();
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const { t } = useTranslation();
  const isOpen = Boolean(anchorEl);

  const sort = searchParams.get("sort") ?? "";
  const displayLabel =
    GOV_ACTION_HISTORY_SORT_OPTIONS.find((option) => option.value === sort)
      ?.displayLabel || sort;

  const setSort = (value: string) => {
    const newParams = getCurrentSearchParams();
    newParams.set("sort", value);
    setSearchParams(newParams);
  };

  return (
    <Box>
      <GovernanceActionHistoryMenuButton
        id="sort-button"
        menuId="sort-menu"
        label={`${t("governanceActionHistoryList.sort.title")}${
          sort ? `: ${displayLabel}` : ""
        }`}
        isOpen={isOpen}
        onClick={(event: MouseEvent<HTMLElement>) =>
          setAnchorEl(event.currentTarget)
        }
      />
      <Menu
        id="sort-menu"
        data-testid="sort-menu"
        anchorEl={anchorEl}
        open={isOpen}
        onClose={() => setAnchorEl(null)}
        TransitionComponent={Fade}
        sx={{ marginTop: 1 }}
      >
        <FormControl>
          <Box px="20px">
            <Typography
              sx={{ fontSize: 14, fontWeight: 500, color: fadedPurple.c500 }}
            >
              {t("governanceActionHistoryList.sort.fullTitle")}
            </Typography>
          </Box>
          <Divider sx={{ marginTop: 1 }} />
          <RadioGroup
            id="sort-radio-buttons-group"
            data-testid="sort-radio-buttons-group"
            aria-labelledby="sort-radio-buttons-group"
            name="sort-radio-buttons-group"
            value={sort}
          >
            {GOV_ACTION_HISTORY_SORT_OPTIONS.map((option) => (
              <Box
                id={`${option.dataTestId}-radio-wrapper`}
                data-testid={`${option.dataTestId}-radio-wrapper`}
                key={option.value}
                paddingX="20px"
                sx={{ cursor: "pointer", "&:hover": { bgcolor: "#E6EBF7" } }}
                bgcolor={sort === option.value ? "#FFF0E7" : "transparent"}
                onClick={() => setSort(option.value)}
              >
                <FormControlLabel
                  value={option.value}
                  control={
                    <Radio
                      id={`${option.dataTestId}-radio`}
                      data-testid={`${option.value.toLocaleLowerCase()}-radio`}
                      onChange={(e) => {
                        e.stopPropagation();
                        setSort(option.value);
                      }}
                      onClick={(e) => e.stopPropagation()}
                    />
                  }
                  label={option.label}
                  onClick={(e) => e.stopPropagation()}
                />
              </Box>
            ))}
          </RadioGroup>
        </FormControl>
      </Menu>
    </Box>
  );
};
