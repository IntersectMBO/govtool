import { Dispatch, SetStateAction, useCallback } from "react";
import {
  Box,
  Checkbox,
  FormControlLabel,
  FormLabel,
  Typography,
} from "@mui/material";

import { GOVERNANCE_ACTIONS_FILTERS } from "@consts";
import { useTranslation } from "@hooks";

interface Props {
  chosenFilters: string[];
  setChosenFilters: Dispatch<SetStateAction<string[]>>;
}

export function GovernanceActionsFilters({
  chosenFilters,
  setChosenFilters,
}: Props) {
  const handleFilterChange = useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      e.target.name, e.target.checked;
      let filters = [...chosenFilters];
      if (e.target.checked) {
        filters.push(e.target.name);
      } else {
        filters = filters.filter((str) => str !== e.target.name);
      }
      setChosenFilters(filters);
    },
    [chosenFilters, setChosenFilters],
  );

  const { t } = useTranslation();

  return (
    <Box
      display="flex"
      flexDirection="column"
      position="absolute"
      sx={{
        background: "#FBFBFF",
        boxShadow: "1px 2px 11px 0px #00123D5E",
        borderRadius: "10px",
        padding: "12px 0px",
        width: "auto",
        zIndex: "1",
      }}
    >
      <FormLabel
        sx={{
          fontSize: 14,
          fontWeight: 500,
          color: "#9792B5",
          paddingX: "20px",
        }}
      >
        {t("govActions.filterTitle")}
      </FormLabel>
      {GOVERNANCE_ACTIONS_FILTERS.map((item) => (
        <Box
          key={item.key}
          paddingX="20px"
          sx={[{ "&:hover": { bgcolor: "#E6EBF7" } }]}
          bgcolor={
              chosenFilters?.includes(item.key) ? "#FFF0E7" : "transparent"
            }
        >
          <FormControlLabel
            control={(
              <Checkbox
                inputProps={{
                  "data-testid": `${item.label.replace(/ /g, "")}-checkbox`,
                }}
                onChange={handleFilterChange}
                name={item.key}
                checked={chosenFilters?.includes(item.key)}
              />
              )}
            label={(
              <Typography fontSize={14} fontWeight={500}>
                {item.label}
              </Typography>
              )}
          />
        </Box>
      ))}
    </Box>
  );
}
