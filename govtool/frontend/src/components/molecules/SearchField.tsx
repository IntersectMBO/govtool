import { Box, ButtonBase, InputBase } from "@mui/material";
import { Search, X } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds, RADIUS } from "@/consts/designTokens";

/** The list pages' search box: icon, input (search-input) and a clear button. */
export const SearchField = ({
  value,
  onChange,
  onClear,
  placeholder,
}: {
  value: string;
  onChange: (next: string) => void;
  /** Shows a clear button while there is text. */
  onClear?: () => void;
  placeholder: string;
}) => {
  const { t } = useTranslation();
  return (
    <Box
      sx={{
      alignItems: "center",
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.md}px`,
      display: "flex",
      flex: { xxs: "1 1 100%", md: 1 },
      gap: 1,
      height: 40,
      minWidth: 0,
      px: 1.5,
      "&:focus-within": { borderColor: ds.primary["600"] },
    }}
    >
      <Search aria-hidden color={ds.gray["500"]} size={16} />
      <InputBase
        inputProps={{ "aria-label": placeholder, "data-testid": "search-input" }}
        onChange={(event) => onChange(event.target.value)}
        placeholder={placeholder}
        sx={{ flex: 1, fontSize: 14, "& input": { p: 0 } }}
        value={value}
      />
      {onClear && value && (
      <ButtonBase
        aria-label={t("governanceActionHistoryList.clearSearchInput")}
        data-testid="clear-search-input"
        onClick={onClear}
        sx={{ borderRadius: "50%", color: ds.gray["500"], height: 24, width: 24 }}
      >
        <X aria-hidden size={14} />
      </ButtonBase>
    )}
    </Box>
  );
};
