import { Box } from "@mui/material";
import { FolderOpen } from "lucide-react";

import { Button } from "@atoms";
import { useTranslation } from "@hooks";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

type Props = {
  /** The search that matched nothing, quoted in the message. */
  searchTerm?: string;
  onClearSearch?: () => void;
  onBrowseAll?: () => void;
};

/** "No DReps found", with ways out when a search or filter caused it. */
export const EmptyStateDrepDirectory = ({
  searchTerm,
  onClearSearch,
  onBrowseAll,
}: Props) => {
  const { t } = useTranslation();

  return (
    <Box
      data-testid="drep-directory-empty-state"
      sx={{
        alignItems: "center",
        backgroundColor: "#FFFFFF",
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.xl}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: "column",
        gap: 1.5,
        px: 3,
        py: { xxs: 6, md: 12 },
        textAlign: "center",
        width: "100%",
      }}
    >
      <Box
        sx={{
          alignItems: "center",
          backgroundColor: ds.primary["50"],
          borderRadius: "50%",
          color: ds.primary["600"],
          display: "flex",
          height: 72,
          justifyContent: "center",
          mb: 1,
          width: 72,
        }}
      >
        <FolderOpen aria-hidden size={28} />
      </Box>
      <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, lineHeight: "28px", m: 0 }}>
        {t("dRepDirectory.noResultsForTheSearchTitle")}
      </Box>
      <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, maxWidth: 440, overflowWrap: "anywhere" }}>
        {searchTerm ? (
          <>
            {t("dRepDirectory.empty.noMatchBefore")}
            <Box component="strong" sx={{ color: ds.gray["900"] }}>{`"${searchTerm}"`}</Box>
            {t("dRepDirectory.empty.noMatchAfter")}
          </>
        ) : (
          t("dRepDirectory.noResultsForTheSearchDescription")
        )}
      </Box>
      {(onClearSearch || onBrowseAll) && (
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "center", mt: 1.5 }}>
          {onClearSearch && searchTerm && (
            <Button dataTestId="clear-search-button" onClick={onClearSearch} variant="outlined">
              {t("dRepDirectory.empty.clearSearch")}
            </Button>
          )}
          {onBrowseAll && (
            <Button dataTestId="browse-all-dreps-button" onClick={onBrowseAll}>
              {t("dRepDirectory.empty.browseAll")}
            </Button>
          )}
        </Box>
      )}
    </Box>
  );
};
