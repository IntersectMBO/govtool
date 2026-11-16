import { KeyboardEvent, useRef } from "react";
import { Box, ButtonBase } from "@mui/material";

import { ds, RADIUS } from "@/consts/designTokens";

type Tab = { value: string; label: string; dataTestId?: string };

type Props = {
  tabs: Tab[];
  value: string;
  onChange: (value: string) => void;
  /** Names the tablist for assistive technology. */
  label: string;
  /** Prefixes the tab and panel ids; one per page. */
  idPrefix: string;
};

const tabId = (idPrefix: string, value: string) => `${idPrefix}-tab-${value}`;
const panelId = (idPrefix: string) => `${idPrefix}-panel`;

/**
 * A grey segmented control that behaves as a tablist: arrow keys, Home and
 * End move between tabs, and only the selected tab is in the tab order.
 */
export const SegmentedTabs = ({ tabs, value, onChange, label, idPrefix }: Props) => {
  const refs = useRef<Record<string, HTMLButtonElement | null>>({});

  const select = (index: number) => {
    const next = tabs[(index + tabs.length) % tabs.length];
    onChange(next.value);
    refs.current[next.value]?.focus();
  };

  const onKeyDown = (event: KeyboardEvent, index: number) => {
    const moves: Record<string, number> = {
      ArrowRight: index + 1,
      ArrowLeft: index - 1,
      Home: 0,
      End: tabs.length - 1,
    };
    if (!(event.key in moves)) return;
    event.preventDefault();
    select(moves[event.key]);
  };

  return (
    <Box
      aria-label={label}
      role="tablist"
      sx={{
        alignSelf: "flex-start",
        backgroundColor: ds.gray["100"],
        borderRadius: `${RADIUS.md}px`,
        display: "flex",
        gap: 0.5,
        p: 0.5,
      }}
    >
      {tabs.map((tab, index) => {
        const isActive = tab.value === value;
        return (
          <ButtonBase
            key={tab.value}
            ref={(node: HTMLButtonElement | null) => {
              refs.current[tab.value] = node;
            }}
            aria-controls={panelId(idPrefix)}
            aria-selected={isActive}
            data-testid={tab.dataTestId}
            id={tabId(idPrefix, tab.value)}
            onClick={() => onChange(tab.value)}
            onKeyDown={(event) => onKeyDown(event, index)}
            role="tab"
            tabIndex={isActive ? 0 : -1}
            sx={{
              backgroundColor: isActive ? "#FFFFFF" : "transparent",
              borderRadius: `${RADIUS.sm}px`,
              boxShadow: isActive ? "0px 1px 2px rgba(16, 24, 40, 0.08)" : "none",
              color: isActive ? ds.gray["900"] : ds.gray["500"],
              fontFamily: "inherit",
              fontSize: 14,
              fontWeight: isActive ? 600 : 500,
              height: 32,
              px: 1.5,
              "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` },
            }}
          >
            {tab.label}
          </ButtonBase>
        );
      })}
    </Box>
  );
};

/** Spread on the element the tabs switch: its role, id and label. */
SegmentedTabs.panelProps = (idPrefix: string, value: string) => ({
  "aria-labelledby": tabId(idPrefix, value),
  id: panelId(idPrefix),
  role: "tabpanel",
});
