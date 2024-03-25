import { Dispatch, SetStateAction, useCallback, useRef } from "react";
import {
  Box,
  Checkbox,
  FormControlLabel,
  FormLabel,
  Typography,
} from "@mui/material";

import { GOVERNANCE_ACTIONS_FILTERS } from "@consts";
import { useOnClickOutside, useScreenDimension, useTranslation } from "@hooks";

interface Props {
  chosenFilters: string[];
  setChosenFilters: Dispatch<SetStateAction<string[]>>;
  closeFilters: () => void;
}

export const GovernanceActionsFilters = ({
  chosenFilters,
  setChosenFilters,
  closeFilters,
}: Props) => {
  const handleFilterChange = useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      // TODO: Refine if it is needed to remove this eslint-disable
      // eslint-disable-next-line no-unused-expressions, no-sequences
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
  const { isMobile, screenWidth } = useScreenDimension();

  const wrapperRef = useRef<HTMLDivElement>(null);
  useOnClickOutside(wrapperRef, closeFilters);

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
        width: screenWidth < 850 ? "315px" : "415px",
        zIndex: "1",
        right: isMobile ? "59px" : "115px",
        top: "53px",
      }}
      ref={wrapperRef}
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
            control={
              <Checkbox
                inputProps={{
                  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
                  // @ts-expect-error
                  "data-testid": `${item.label.replace(/ /g, "")}-checkbox`,
                }}
                onChange={handleFilterChange}
                name={item.key}
                checked={chosenFilters?.includes(item.key)}
              />
            }
            label={
              <Typography fontSize={14} fontWeight={500}>
                {item.label}
              </Typography>
            }
          />
        </Box>
      ))}
    </Box>
  );
};
