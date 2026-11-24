import { InputHTMLAttributes, useState } from "react";
import {
  Box,
  ButtonBase,
  FormControlLabel,
  Popover,
  Radio,
  RadioGroup,
} from "@mui/material";
import { ChevronDown } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

type SortOption = { key: string; label: string };

type Props = {
  options: SortOption[];
  /** The chosen key; "" is the backend's default order. */
  value: string;
  onChange: (next: string) => void;
  /** The prefix before the choice; "Sort:" by default. */
  label?: string;
  /** Shown when no option matches `value`; "Default" by default. */
  placeholder?: string;
  dataTestId?: string;
};

/**
 * "Sort: Newest" button opening a radio list; keeps the old sort test ids.
 * With `label` it serves other single-choice filters ("Status: All").
 */
export const SortSelect = ({
  options,
  value,
  onChange,
  label,
  placeholder,
  dataTestId = "sort-button",
}: Props) => {
  const { t } = useTranslation();
  const prefix = label ?? t("listControls.sortLabel");
  const [anchor, setAnchor] = useState<HTMLElement | null>(null);
  const current = options.find((option) => option.key === value);

  return (
    <>
      <ButtonBase
        aria-expanded={Boolean(anchor)}
        aria-haspopup="true"
        data-testid={dataTestId}
        onClick={(event) => setAnchor(event.currentTarget)}
        sx={{
          alignItems: "center",
          backgroundColor: "#FFFFFF",
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: `${RADIUS.md}px`,
          color: ds.gray["500"],
          display: "inline-flex",
          flexShrink: 0,
          fontFamily: "inherit",
          fontSize: 14,
          gap: 0.75,
          height: 40,
          letterSpacing: "-0.09px",
          px: 1.5,
          whiteSpace: "nowrap",
          "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` },
        }}
      >
        {prefix}
        <Box component="span" sx={{ color: ds.gray["900"], fontWeight: 500 }}>
          {current?.label ?? placeholder ?? t("listControls.sortDefault")}
        </Box>
        <ChevronDown aria-hidden color={ds.gray["700"]} size={16} />
      </ButtonBase>
      <Popover
        anchorEl={anchor}
        anchorOrigin={{ horizontal: "right", vertical: "bottom" }}
        onClose={() => setAnchor(null)}
        open={Boolean(anchor)}
        slotProps={{
          paper: {
            sx: {
              border: `1px solid ${ds.gray["200"]}`,
              borderRadius: `${RADIUS.md}px`,
              boxShadow: SHADOWS.drop,
              mt: 0.5,
              px: 2,
              py: 1,
            },
          },
        }}
        transformOrigin={{ horizontal: "right", vertical: "top" }}
      >
        <RadioGroup
          aria-label={prefix}
          onChange={(event) => {
            onChange(event.target.value);
            setAnchor(null);
          }}
          value={value}
        >
          {options.map((option) => (
            <FormControlLabel
              key={option.key}
              control={
                <Radio
                  inputProps={
                    {
                      "data-testid": `${option.key}-radio`,
                    } as InputHTMLAttributes<HTMLInputElement>
                  }
                  size="small"
                />
              }
              label={option.label}
              slotProps={{ typography: { fontSize: 14 } }}
              value={option.key}
            />
          ))}
        </RadioGroup>
      </Popover>
    </>
  );
};
