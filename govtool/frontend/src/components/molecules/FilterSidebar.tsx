import { ReactNode } from "react";
import { Box, ButtonBase } from "@mui/material";

import { Check } from "lucide-react";

import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

export type FilterSidebarOption = {
  key: string;
  label: string;
  icon?: ReactNode;
  count?: number;
  /** The label's colour, such as a status colour. */
  color?: string;
  /** Names the row's test id when the shown label is a short form. */
  testLabel?: string;
};

type Props = {
  title?: string;
  allLabel: string;
  allIcon?: ReactNode;
  allCount?: number;
  options: FilterSidebarOption[];
  /** Selected keys; none selected means "all". */
  selected: string[];
  onChange: (next: string[]) => void;
  dataTestId?: string;
  /** Rows are checkboxes with a box at the end, as the status filter draws them. */
  checkboxes?: boolean;
};

const testIdFor = (label: string) => `${label.replace(/ /g, "")}-checkbox`;

const Box16 = ({ isChecked }: { isChecked: boolean }) => (
  <Box
    aria-hidden
    component="span"
    sx={{
      alignItems: "center",
      backgroundColor: isChecked ? ds.primary["600"] : "#FFFFFF",
      border: `1px solid ${isChecked ? ds.primary["600"] : ds.gray["300"]}`,
      borderRadius: "4px",
      color: "#FFFFFF",
      display: "inline-flex",
      flexShrink: 0,
      height: 16,
      justifyContent: "center",
      width: 16,
    }}
  >
    {isChecked && <Check size={12} strokeWidth={3} />}
  </Box>
);

const Row = ({
  icon,
  label,
  count,
  color,
  isSelected,
  onClick,
  dataTestId,
  checkbox,
}: {
  icon?: ReactNode;
  label: string;
  count?: number;
  color?: string;
  isSelected: boolean;
  onClick: () => void;
  dataTestId: string;
  /** Draw a checkbox and report checked; otherwise a pressed toggle. */
  checkbox?: { isChecked: boolean };
}) => (
  <ButtonBase
    aria-checked={checkbox ? checkbox.isChecked : undefined}
    aria-pressed={checkbox ? undefined : isSelected}
    data-testid={dataTestId}
    onClick={onClick}
    role={checkbox ? "checkbox" : undefined}
    sx={{
      alignItems: "center",
      backgroundColor: isSelected ? ds.gray["100"] : "transparent",
      borderRadius: `${RADIUS.sm}px`,
      color: ds.gray["900"],
      display: "flex",
      fontFamily: "inherit",
      fontSize: 14,
      fontWeight: isSelected ? 600 : 400,
      gap: 1,
      justifyContent: "flex-start",
      letterSpacing: "-0.09px",
      lineHeight: "20px",
      px: 1,
      py: 0.875,
      textAlign: "left",
      width: "100%",
      "&:hover": { backgroundColor: ds.gray["50"] },
      "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` },
    }}
  >
    {icon && (
      <Box component="span" sx={{ display: "inline-flex", flexShrink: 0 }}>
        {icon}
      </Box>
    )}
    <Box component="span" sx={{ color, flex: 1, minWidth: 0 }}>
      {label}
    </Box>
    {count !== undefined && (
      <Box
        component="span"
        sx={{
          backgroundColor: "#FFFFFF",
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: `${RADIUS.pill}px`,
          color: ds.gray["700"],
          fontSize: 11,
          fontWeight: 500,
          lineHeight: "16px",
          minWidth: 22,
          px: 0.75,
          textAlign: "center",
        }}
      >
        {count}
      </Box>
    )}
    {checkbox && <Box16 isChecked={checkbox.isChecked} />}
  </ButtonBase>
);

/**
 * A filter card for the list sidebars: an "all" row and one row per option.
 * Options toggle; the "all" row clears the selection.
 */
export const FilterSidebar = ({
  title,
  allLabel,
  allIcon,
  allCount,
  options,
  selected,
  onChange,
  dataTestId,
  checkboxes = false,
}: Props) => (
  <Box
    component="section"
    data-testid={dataTestId}
    sx={{
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      boxShadow: SHADOWS.card,
      display: "flex",
      flexDirection: "column",
      gap: 0.25,
      p: 0.75,
    }}
  >
    {title && (
      <Box
        component="h2"
        sx={{
          color: ds.gray["500"],
          fontSize: 11,
          fontWeight: 500,
          letterSpacing: "0.4px",
          lineHeight: "16px",
          m: 0,
          px: 1,
          py: 1,
          textTransform: "uppercase",
        }}
      >
        {title}
      </Box>
    )}
    <Row
      // The all row clears the choice; its box stays empty, as in the design.
      checkbox={checkboxes ? { isChecked: false } : undefined}
      count={allCount}
      dataTestId={testIdFor(allLabel)}
      icon={allIcon}
      isSelected={selected.length === 0}
      label={allLabel}
      onClick={() => onChange([])}
    />
    {options.map((option) => (
      <Row
        key={option.key}
        checkbox={checkboxes ? { isChecked: selected.includes(option.key) } : undefined}
        color={option.color}
        count={option.count}
        dataTestId={testIdFor(option.testLabel ?? option.label)}
        icon={option.icon}
        isSelected={selected.includes(option.key)}
        label={option.label}
        onClick={() =>
          onChange(
            selected.includes(option.key)
              ? selected.filter((key) => key !== option.key)
              : [...selected, option.key],
          )
        }
      />
    ))}
  </Box>
);
