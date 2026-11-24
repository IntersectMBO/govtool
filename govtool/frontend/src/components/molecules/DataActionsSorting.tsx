import { Dispatch, SetStateAction, useRef } from "react";
import {
  Box,
  FormControl,
  FormControlLabel,
  Radio,
  RadioGroup,
  Typography,
} from "@mui/material";

import { useTranslation, useOnClickOutside } from "@hooks";

import { ds } from "@/consts/designTokens";

interface Props {
  chosenSorting: string;
  setChosenSorting: Dispatch<SetStateAction<string>>;
  closeSorts: () => void;
  options: {
    key: string;
    label: string;
  }[];
}

export const DataActionsSorting = ({
  chosenSorting,
  setChosenSorting,
  closeSorts,
  options,
}: Props) => {
  const { t } = useTranslation();

  const wrapperRef = useRef<HTMLDivElement>(null);
  useOnClickOutside(wrapperRef, closeSorts);

  return (
    <Box
      display="flex"
      flexDirection="column"
      position="absolute"
      sx={{
        background: ds.gray["25"],
        boxShadow: "1px 2px 11px 0px #00123D5E",
        borderRadius: "10px",
        padding: "12px 0px",
        width: {
          xxs: "250px",
          md: "415px",
        },
        zIndex: "1",
        right: "3px",
        top: "53px",
      }}
      ref={wrapperRef}
    >
      <FormControl>
        <Box display="flex" justifyContent="space-between" px="20px">
          <Typography sx={{ fontSize: 14, fontWeight: 500, color: ds.gray["500"] }}>
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
          {options.map((item) => (
            <FormControlLabel
              sx={[
                {
                  margin: 0,
                  px: "20px",
                  bgcolor:
                    chosenSorting === item.key ? ds.orange["50"] : "transparent",
                },
                { "&:hover": { bgcolor: ds.gray["200"] } },
              ]}
              key={item.key}
              value={item.key}
              control={
                // TODO: Fix typing of inputProps
                // eslint-disable-next-line @typescript-eslint/ban-ts-comment
                // @ts-expect-error
                <Radio inputProps={{ "data-testid": `${item.key}-radio` }} />
              }
              label={item.label}
            />
          ))}
        </RadioGroup>
      </FormControl>
    </Box>
  );
};
