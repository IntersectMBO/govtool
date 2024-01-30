import { Dispatch, SetStateAction } from "react";
import {
  Box,
  FormControl,
  FormControlLabel,
  Radio,
  RadioGroup,
  Typography,
} from "@mui/material";

import { GOVERNANCE_ACTIONS_SORTING } from "@consts";
import { useTranslation } from "@hooks";

interface Props {
  chosenSorting: string;
  setChosenSorting: Dispatch<SetStateAction<string>>;
}

export const GovernanceActionsSorting = ({
  chosenSorting,
  setChosenSorting,
}: Props) => {
  const { t } = useTranslation();

  return (
    <Box
      display={"flex"}
      flexDirection={"column"}
      position={"absolute"}
      sx={{
        background: "#FBFBFF",
        boxShadow: "1px 2px 11px 0px #00123D5E",
        borderRadius: "10px",
        padding: "12px 0px",
        width: "auto",
        zIndex: "1",
      }}
    >
      <FormControl>
        <Box display="flex" justifyContent="space-between" px={"20px"}>
          <Typography sx={{ fontSize: 14, fontWeight: 500, color: "#9792B5" }}>
            {t("sortBy")}
          </Typography>
          <Box sx={{ cursor: "pointer" }} onClick={() => setChosenSorting("")}>
            <Typography fontSize={14} fontWeight={500} color="primary">
              {t("clear")}
            </Typography>
          </Box>
        </Box>
        <RadioGroup
          aria-labelledby="demo-controlled-radio-buttons-group"
          name="controlled-radio-buttons-group"
          value={chosenSorting}
          onChange={(e) => {
            setChosenSorting(e.target.value);
          }}
        >
          {GOVERNANCE_ACTIONS_SORTING.map((item) => {
            return (
              <FormControlLabel
                sx={[
                  {
                    margin: 0,
                    px: "20px",
                    bgcolor:
                      chosenSorting === item.key ? "#FFF0E7" : "transparent",
                  },
                  { "&:hover": { bgcolor: "#E6EBF7" } },
                ]}
                key={item.key}
                value={item.key}
                control={
                  <Radio inputProps={{ "data-testid": item.key + "-radio" }} />
                }
                label={item.label}
              />
            );
          })}
        </RadioGroup>
      </FormControl>
    </Box>
  );
};
