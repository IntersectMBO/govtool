import { useEffect, useState } from "react";
import { IconButton, Tooltip } from "@mui/material";
import { Search } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds, RADIUS } from "@/consts/designTokens";

import { GlobalSearchDialog } from "./GlobalSearchDialog";

/** The header's search button; Ctrl+K or ⌘K opens the dialog from anywhere. */
export const GlobalSearchButton = () => {
  const { t } = useTranslation();
  const [isOpen, setIsOpen] = useState(false);

  useEffect(() => {
    // ⌘K on a Mac (where Ctrl+K edits text), Ctrl+K elsewhere; never with
    // Shift or Alt (browser shortcuts), nor over another open dialog.
    const isMac = /Mac|iPhone|iPad/.test(navigator.platform);
    const onKeyDown = (event: KeyboardEvent) => {
      const modifier = isMac ? event.metaKey : event.ctrlKey;
      if (!modifier || event.shiftKey || event.altKey || event.defaultPrevented) return;
      if (event.key.toLowerCase() !== "k") return;
      if (document.querySelector('[role="dialog"]')) return;
      event.preventDefault();
      setIsOpen(true);
    };
    window.addEventListener("keydown", onKeyDown);
    return () => window.removeEventListener("keydown", onKeyDown);
  }, []);

  return (
    <>
      <Tooltip
        componentsProps={{
          tooltip: {
            sx: {
              backgroundColor: "#FFFFFF",
              border: `1px solid ${ds.gray["200"]}`,
              borderRadius: `${RADIUS.sm}px`,
              boxShadow: "0px 4px 8px rgba(16, 24, 40, 0.06)",
              color: ds.gray["700"],
              fontSize: 14,
              px: 1.5,
              py: 0.75,
            },
          },
        }}
        title={t("globalSearch.button")}
      >
        <IconButton
          aria-haspopup="dialog"
          aria-label={t("globalSearch.button")}
          data-testid="global-search-button"
          onClick={() => setIsOpen(true)}
          sx={{
            backgroundColor: "#FFFFFF",
            border: `1px solid ${ds.gray["200"]}`,
            borderRadius: "50%",
            color: ds.gray["700"],
            height: 36,
            width: 36,
            "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
          }}
        >
          <Search aria-hidden size={16} strokeWidth={1.75} />
        </IconButton>
      </Tooltip>
      <GlobalSearchDialog onClose={() => setIsOpen(false)} open={isOpen} />
    </>
  );
};
