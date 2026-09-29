import { MouseEvent, useState } from "react";
import {
  Box,
  Checkbox,
  Divider,
  Fade,
  FormControlLabel,
  Menu,
} from "@mui/material";
import { useSearchParams } from "react-router";

import { Button, Typography } from "@atoms";
import {
  fadedPurple,
  OUTCOMES_STATUS_FILTERS,
  OUTCOMES_TYPE_FILTERS,
} from "@consts";
import { useTranslation } from "@hooks";
import { getCurrentSearchParams } from "@utils";

import { OutcomesMenuButton } from "./OutcomesMenuButton";

type FilterParam = "type" | "status";

const getValues = (params: URLSearchParams, name: FilterParam) =>
  params.get(name)?.split(",").filter(Boolean) ?? [];

/** Type and status filters, kept in the `type` and `status` URL parameters. */
export const OutcomesFilters = () => {
  const [searchParams, setSearchParams] = useSearchParams();
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const { t } = useTranslation();
  const isOpen = Boolean(anchorEl);

  const isChecked = (name: FilterParam, value: string) =>
    getValues(searchParams, name).includes(value);

  const toggle = (name: FilterParam, value: string) => {
    const newParams = getCurrentSearchParams();
    const current = getValues(newParams, name);
    const updated = current.includes(value)
      ? current.filter((item) => item !== value)
      : [...current, value];

    if (updated.length) {
      newParams.set(name, updated.join(","));
    } else {
      newParams.delete(name);
    }
    setSearchParams(newParams);
  };

  const clearFilters = () => {
    const newParams = getCurrentSearchParams();
    newParams.delete("type");
    newParams.delete("status");
    setSearchParams(newParams);
  };

  const activeFilterCount =
    getValues(searchParams, "type").length +
    getValues(searchParams, "status").length;

  const renderGroupTitle = (title: string) => (
    <Typography sx={{ color: fadedPurple.c500, fontWeight: 500, fontSize: 14 }}>
      {title}
    </Typography>
  );

  const renderOption = (
    name: FilterParam,
    value: string,
    label: string,
    testId: string,
  ) => (
    <Box
      id={`${testId}-checkbox-wrapper`}
      data-testid={`${testId}-checkbox-wrapper`}
      key={value}
      paddingX="20px"
      sx={{ cursor: "pointer", "&:hover": { bgcolor: "#E6EBF7" } }}
      bgcolor={isChecked(name, value) ? "#FFF0E7" : "transparent"}
      onClick={() => toggle(name, value)}
    >
      <FormControlLabel
        control={
          <Checkbox
            id={`${testId}-checkbox`}
            data-testid={`${testId}-checkbox`}
            checked={isChecked(name, value)}
            onChange={(e) => {
              e.stopPropagation();
              toggle(name, value);
            }}
            onClick={(e) => e.stopPropagation()}
            name={value}
          />
        }
        label={label}
        onClick={(e) => e.stopPropagation()}
      />
    </Box>
  );

  return (
    <Box>
      <OutcomesMenuButton
        id="filters-button"
        menuId="filters-menu"
        label={t("outcomesList.filters.title")}
        isOpen={isOpen}
        badgeCount={activeFilterCount}
        onClick={(event: MouseEvent<HTMLElement>) =>
          setAnchorEl(event.currentTarget)
        }
      />
      <Menu
        id="filters-menu"
        data-testid="filters-menu"
        anchorEl={anchorEl}
        open={isOpen}
        onClose={() => setAnchorEl(null)}
        TransitionComponent={Fade}
        sx={{ marginTop: 1 }}
      >
        <Box
          display="flex"
          paddingX="20px"
          justifyContent="space-between"
          alignItems="center"
        >
          {renderGroupTitle(t("outcome.governanceActionType"))}
          <Button
            id="clear-filters-button"
            data-testid="clear-filters-button"
            variant="text"
            size="small"
            sx={{ px: 1 }}
            onClick={clearFilters}
          >
            {t("outcomesList.filters.clear")}
          </Button>
        </Box>
        <Divider sx={{ marginTop: 1 }} />
        {OUTCOMES_TYPE_FILTERS.map((option) =>
          renderOption("type", option.value, option.label, option.dataTestId),
        )}
        <Box paddingX="20px" width={250}>
          {renderGroupTitle(t("outcome.status.fullTitle"))}
        </Box>
        <Divider sx={{ marginTop: 1 }} />
        {OUTCOMES_STATUS_FILTERS.map((option) =>
          renderOption("status", option.value, option.label, option.value),
        )}
      </Menu>
    </Box>
  );
};
