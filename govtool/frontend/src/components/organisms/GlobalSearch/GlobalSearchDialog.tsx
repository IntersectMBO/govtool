import { KeyboardEvent, useEffect, useRef, useState } from "react";
import { Box, ButtonBase, Dialog, InputBase } from "@mui/material";
import { useNavigate } from "react-router";
import { CloudOff, History, Pencil, RefreshCw, Search, SearchX, X } from "lucide-react";

import { Button } from "@atoms";
import { GOV_ACTION_HISTORY_PATHS, PATHS, PDF_PATHS } from "@consts";
import { useFeatureFlag, useSnackbar } from "@context";
import { useDebounce, useTranslation } from "@hooks";
import { useGlobalSearch, type SearchCategory } from "@/hooks/queries/useGlobalSearch";
import { ds, RADIUS } from "@/consts/designTokens";
import {
  clearRecentSearches,
  readRecentSearches,
  saveRecentSearch,
  type RecentSearch,
} from "@/utils/globalSearch";

import { CategoryChip, Kbd, ResultRow, ResultSkeleton, StatePanel } from "./searchParts";
import { CATEGORY_ORDER, SearchResult, toResults } from "./searchResults";

const LISTBOX_ID = "global-search-listbox";
const optionId = (index: number) => `global-search-option-${index}`;

const isMac = () =>
  typeof navigator !== "undefined" && /Mac|iPhone|iPad/.test(navigator.platform);

const BROWSE_PATHS: Partial<Record<SearchCategory, string>> = {
  actions: PATHS.governanceActions,
  dreps: PATHS.dRepDirectory,
  proposals: PDF_PATHS.proposalDiscussion,
  outcomes: GOV_ACTION_HISTORY_PATHS.governanceActionHistory,
};

const SearchPanel = ({ onClose }: { onClose: () => void }) => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { addSuccessAlert } = useSnackbar();

  const [query, setQuery] = useState("");
  const [picked, setPicked] = useState<SearchCategory | null>(null);
  // The default tab, kept once it has shown results for a search text, so a
  // slower type arriving later does not move the list under the user.
  const [settled, setSettled] = useState<{ text: string; category: SearchCategory } | null>(
    null,
  );
  const [highlight, setHighlight] = useState(0);
  const [showAddressHint, setShowAddressHint] = useState(false);
  const [recent, setRecent] = useState<RecentSearch[]>(readRecentSearches);
  const inputRef = useRef<HTMLInputElement>(null);

  const debouncedQuery = useDebounce(query, 300);
  // Clearing the field shows browse at once, without waiting out the debounce.
  const debounced = query.trim() ? debouncedQuery : "";
  const search = useGlobalSearch(debounced, {
    isForumEnabled: isProposalDiscussionForumEnabled,
  });
  const results = toResults(search, { t });
  const categories = CATEGORY_ORDER.filter(
    (category) => category !== "proposals" || isProposalDiscussionForumEnabled,
  );
  const enabled = categories.filter((category) => search[category].isEnabled);
  const firstWithResults = enabled.find((category) => results[category].length > 0);
  const active: SearchCategory =
    picked ??
    (settled?.text === search.classified.text ? settled.category : undefined) ??
    firstWithResults ??
    enabled[0] ??
    "actions";
  const state = search[active];
  const rows = results[active];
  // A handle lookup never fails (a miss and an outage both read as no handle).
  const failable = enabled.filter((category) => category !== "handles");
  const allFailed = failable.length > 0 && failable.every((category) => search[category].isError);
  // Until the user picks a type, an empty first type is not "no matches" while
  // another type may still bring results and take over.
  const isWaitingForOthers =
    !picked && rows.length === 0 && enabled.some((category) => search[category].isLoading);

  useEffect(() => {
    if (!picked && firstWithResults && settled?.text !== search.classified.text) {
      setSettled({ text: search.classified.text, category: firstWithResults });
    }
  }, [picked, firstWithResults, settled, search.classified.text]);
  // A new kind of query (words, an address, a handle) starts from its own default.
  const { kind } = search.classified;
  const lastKind = useRef(kind);
  useEffect(() => {
    if (lastKind.current !== kind && picked !== "addresses") setPicked(null);
    lastKind.current = kind;
  }, [kind]);
  useEffect(() => setHighlight(0), [debounced, active]);
  useEffect(() => {
    document.getElementById(optionId(highlight))?.scrollIntoView?.({ block: "nearest" });
  }, [highlight]);

  const focusInput = (select = false) => {
    inputRef.current?.focus();
    if (select) inputRef.current?.select();
  };

  const clear = () => {
    setQuery("");
    setPicked(null);
    focusInput();
  };

  const open = (result: SearchResult) => {
    setRecent(saveRecentSearch({ query: search.classified.text, kind: search.classified.kind }));
    const { target } = result;
    if (target.kind === "navigate") {
      onClose();
      navigate(target.path, { state: target.state });
    } else if (target.kind === "copy") {
      navigator.clipboard?.writeText(target.value);
      addSuccessAlert(t("alerts.copiedToClipboard"));
    } else {
      setQuery(target.query);
      setPicked("addresses");
      focusInput();
    }
  };

  const onKeyDown = (event: KeyboardEvent) => {
    if (!showsList) return;
    if (event.key === "ArrowDown" || event.key === "ArrowUp") {
      event.preventDefault();
      const step = event.key === "ArrowDown" ? 1 : -1;
      setHighlight((current) => (current + step + rows.length) % rows.length);
    } else if (event.key === "Enter") {
      event.preventDefault();
      open(rows[highlight] ?? rows[0]);
    }
  };

  const showsList =
    search.isSearching &&
    !allFailed &&
    state.isEnabled &&
    !state.isLoading &&
    !isWaitingForOthers &&
    !state.isError &&
    !state.isSearchNotReady &&
    rows.length > 0;

  const count = (() => {
    if (state.total !== undefined) return t("globalSearch.matches", { count: state.total });
    if (state.hasMore) return t("globalSearch.matchesMore", { count: rows.length });
    return t("globalSearch.matches", { count: rows.length });
  })();
  const categoryCount =
    state.total ?? (state.hasMore ? `${rows.length}+` : String(rows.length));

  const renderBody = () => {
    if (allFailed) {
      return (
        <StatePanel
          actions={(
            <>
              <Button
                dataTestId="global-search-try-again"
                onClick={() => enabled.forEach((category) => search[category].retry())}
                startIcon={<RefreshCw aria-hidden size={16} />}
              >
                {t("globalSearch.tryAgain")}
              </Button>
              <Button onClick={clear} startIcon={<X aria-hidden size={16} />} variant="outlined">
                {t("globalSearch.clear")}
              </Button>
            </>
          )}
          body={t("globalSearch.errorBody")}
          dataTestId="global-search-error"
          icon={<CloudOff size={26} strokeWidth={1.6} />}
          title={t("globalSearch.errorTitle")}
          tone="error"
        />
      );
    }
    if (!state.isEnabled) {
      return (
        <StatePanel
          body={t(active === "addresses" ? "globalSearch.addressHint" : "globalSearch.handleHint")}
          dataTestId="global-search-category-hint"
          icon={<Search size={26} strokeWidth={1.6} />}
          title={t(`globalSearch.categories.${active}`)}
          tone="primary"
        />
      );
    }
    if (state.isLoading || isWaitingForOthers) {
      return (
        <Box aria-busy sx={{ display: "flex", flexDirection: "column", gap: 1.25 }}>
          <Box
            role="status"
            sx={{ alignItems: "center", backgroundColor: ds.gray["50"], borderRadius: `${RADIUS.lg}px`, display: "flex", gap: 1.25, justifyContent: "space-between", px: 2, py: 1.5 }}
          >
            <Box component="span" sx={{ color: ds.gray["700"], fontSize: 13, fontWeight: 500 }}>
              {t("globalSearch.loading", { count: enabled.length, query: search.classified.text })}
            </Box>
            <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12 }}>
              {t("globalSearch.loadingRight")}
            </Box>
          </Box>
          {[0, 1, 2].map((key) => <ResultSkeleton key={key} />)}
        </Box>
      );
    }
    if (state.isSearchNotReady || state.isError) {
      return (
        <StatePanel
          actions={(
            <Button onClick={state.retry} startIcon={<RefreshCw aria-hidden size={16} />}>
              {t("globalSearch.tryAgain")}
            </Button>
          )}
          body={t(state.isSearchNotReady ? "globalSearch.notReady" : "globalSearch.categoryError")}
          dataTestId="global-search-category-error"
          icon={<CloudOff size={26} strokeWidth={1.6} />}
          title={t(`globalSearch.categories.${active}`)}
          tone="error"
        />
      );
    }
    if (!rows.length) {
      const noStakePart = active === "addresses" && !search.classified.stakeAddress;
      return (
        <StatePanel
          actions={(
            <>
              <Button onClick={() => focusInput(true)} startIcon={<Pencil aria-hidden size={16} />}>
                {t("globalSearch.modify")}
              </Button>
              <Button onClick={clear} startIcon={<X aria-hidden size={16} />} variant="outlined">
                {t("globalSearch.clear")}
              </Button>
            </>
          )}
          body={t(noStakePart ? "globalSearch.noStakePart" : "globalSearch.noMatchesBody")}
          dataTestId="global-search-no-matches"
          icon={<SearchX size={26} strokeWidth={1.6} />}
          title={t("globalSearch.noMatchesTitle", { query: search.classified.text })}
          tone="primary"
        />
      );
    }
    return (
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1 }}>
        <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
          <Box component="h3" id="global-search-results-title" sx={{ color: ds.gray["900"], fontSize: 13, fontWeight: 600, lineHeight: "19.5px", m: 0 }}>
            {t(`globalSearch.categories.${active}`)}
          </Box>
          <Box component="span" sx={{ backgroundColor: ds.gray["100"], borderRadius: `${RADIUS.lg}px`, color: ds.gray["600"], fontSize: 12, fontWeight: 500, lineHeight: "18px", px: 0.75 }}>
            {categoryCount}
          </Box>
        </Box>
        <Box
          aria-labelledby="global-search-results-title"
          component="ul"
          id={LISTBOX_ID}
          role="listbox"
          sx={{ display: "flex", flexDirection: "column", gap: 1, listStyle: "none", m: 0, p: 0 }}
        >
          {rows.map((row, index) => (
            <ResultRow
              key={row.key}
              badge={row.badge}
              category={row.category}
              description={row.description}
              id={optionId(index)}
              isHighlighted={index === highlight}
              meta={row.meta}
              onHighlight={() => setHighlight(index)}
              onOpen={() => open(row)}
              title={row.title}
            />
          ))}
        </Box>
        {state.hasMore && (
          <ButtonBase
            data-testid="global-search-load-more"
            disabled={state.isFetchingMore}
            onClick={state.loadMore}
            sx={{
              backgroundColor: ds.primary["100"],
              border: `1px solid ${ds.primary["200"]}`,
              borderRadius: `${RADIUS.lg}px`,
              color: ds.primary["700"],
              fontFamily: "inherit",
              fontSize: 14,
              fontWeight: 600,
              py: 1.25,
              "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
            }}
          >
            {t("globalSearch.loadMore")}
          </ButtonBase>
        )}
      </Box>
    );
  };

  const renderBrowse = () => (
    <Box
      sx={{
        display: "grid",
        gap: { xxs: 3, md: 4 },
        gridTemplateColumns: { xxs: "1fr", md: "minmax(0, 1fr) minmax(0, 1fr)" },
        pt: 1,
      }}
    >
      {recent.length > 0 && (
        <Box sx={{ borderRight: { md: `1px solid ${ds.gray["200"]}` }, pr: { md: 4 } }}>
          <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between", mb: 1 }}>
            <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, m: 0 }}>
              {t("globalSearch.recent")}
            </Box>
            <ButtonBase
              data-testid="global-search-clear-recent"
              onClick={() => {
                clearRecentSearches();
                setRecent([]);
              }}
              sx={{ borderRadius: "4px", color: ds.primary["600"], fontFamily: "inherit", fontSize: 13, fontWeight: 600, textDecoration: "underline", "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
            >
              {t("globalSearch.clearAll")}
            </ButtonBase>
          </Box>
          <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0 }}>
            {recent.map((item) => (
              <Box key={item.query} component="li">
                <ButtonBase
                  data-testid="global-search-recent"
                  onClick={() => {
                    setQuery(item.query);
                    focusInput();
                  }}
                  sx={{ alignItems: "center", borderRadius: `${RADIUS.md}px`, color: ds.gray["900"], display: "flex", fontFamily: "inherit", fontSize: 14, fontWeight: 500, gap: 1.25, justifyContent: "flex-start", px: 0.5, py: 1, width: "100%", "&:hover": { backgroundColor: ds.gray["50"] }, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
                >
                  <History aria-hidden color={ds.gray["500"]} size={18} strokeWidth={1.5} />
                  <Box component="span" sx={{ flex: 1, minWidth: 0, overflow: "hidden", textAlign: "left", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                    {item.query}
                  </Box>
                  <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12 }}>
                    {t(`globalSearch.kinds.${item.kind}`)}
                  </Box>
                </ButtonBase>
              </Box>
            ))}
          </Box>
        </Box>
      )}
      <Box sx={{ gridColumn: recent.length ? undefined : "1 / -1" }}>
        <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, m: 0, mb: 1.5 }}>
          {t("globalSearch.browse")}
        </Box>
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1 }}>
          {categories.map((category) => (
            <CategoryChip
              key={category}
              category={category}
              dataTestId={`global-search-browse-${category}`}
              label={t(`globalSearch.categories.${category}`)}
              onClick={() => {
                const path = BROWSE_PATHS[category];
                if (path) {
                  onClose();
                  navigate(path);
                } else if (category === "handles") {
                  setQuery("$");
                  focusInput();
                } else {
                  setShowAddressHint(true);
                  focusInput();
                }
              }}
            />
          ))}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 13, lineHeight: "19.5px", m: 0, mt: 1.5 }}>
          {t(showAddressHint ? "globalSearch.addressHint" : "globalSearch.browseHint")}
        </Box>
      </Box>
    </Box>
  );

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2, minHeight: 0 }}>
      <Box sx={{ alignItems: "flex-start", display: "flex", gap: 2, justifyContent: "space-between" }}>
        <Box>
          <Box component="h2" id="global-search-title" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, letterSpacing: "-0.33px", lineHeight: "30px", m: 0 }}>
            {t("globalSearch.title")}
          </Box>
          <Box component="p" sx={{ color: ds.gray["600"], fontSize: 13, lineHeight: "19.5px", m: 0, mt: 0.375 }}>
            {t("globalSearch.description")}
          </Box>
        </Box>
        <ButtonBase
          aria-label={t("globalSearch.close")}
          data-testid="global-search-close"
          onClick={onClose}
          sx={{ backgroundColor: ds.gray["100"], borderRadius: "50%", color: ds.gray["600"], flexShrink: 0, height: 26, width: 26, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
        >
          <X aria-hidden size={14} strokeWidth={2} />
        </ButtonBase>
      </Box>

      <Box
        sx={{
          alignItems: "center",
          border: `1.5px solid ${ds.gray["200"]}`,
          borderRadius: `${RADIUS.lg}px`,
          display: "flex",
          gap: 1.5,
          px: 2,
          py: 0.75,
          "&:focus-within": { borderColor: ds.primary["600"], boxShadow: `0 0 0 3px ${ds.primary["100"]}` },
        }}
      >
        <Search aria-hidden color={ds.primary["600"]} size={20} strokeWidth={2} />
        <InputBase
          autoFocus
          inputProps={{
            "aria-activedescendant": showsList ? optionId(highlight) : undefined,
            "aria-autocomplete": "list",
            "aria-controls": showsList ? LISTBOX_ID : undefined,
            "aria-expanded": showsList,
            "aria-label": t("globalSearch.inputLabel"),
            "data-testid": "global-search-input",
            role: "combobox",
          }}
          inputRef={inputRef}
          onChange={(event) => {
            setQuery(event.target.value);
            setShowAddressHint(false);
          }}
          onKeyDown={onKeyDown}
          placeholder={t("globalSearch.placeholder")}
          sx={{ color: ds.gray["900"], flex: 1, fontSize: 14, fontWeight: 500, "& input": { p: "6px 0" } }}
          value={query}
        />
        {query ? (
          <ButtonBase
            aria-label={t("globalSearch.clear")}
            data-testid="global-search-clear"
            onClick={clear}
            sx={{ borderRadius: "50%", color: ds.gray["600"], height: 26, width: 26, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
          >
            <X aria-hidden size={14} strokeWidth={2} />
          </ButtonBase>
        ) : (
          <Kbd>{isMac() ? "⌘ K" : "Ctrl + K"}</Kbd>
        )}
      </Box>

      {search.isSearching ? (
        <>
          <Box aria-label={t("globalSearch.categoriesLabel")} role="group" sx={{ display: "flex", flexWrap: "wrap", gap: 1 }}>
            {categories.map((category) => (
              <CategoryChip
                key={category}
                category={category}
                dataTestId={`global-search-category-${category}`}
                isActive={category === active}
                label={t(`globalSearch.categories.${category}`)}
                onClick={() => setPicked(category)}
                pressed={category === active}
              />
            ))}
          </Box>
          <Box sx={{ minHeight: 0, overflowY: "auto", pr: 0.5 }}>{renderBody()}</Box>
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between" }}>
            <Box aria-live="polite" component="span" data-testid="global-search-count" sx={{ color: ds.gray["600"], fontSize: 13, fontWeight: 500 }}>
              {state.isEnabled && !state.isLoading && !state.isError && !isWaitingForOthers
                ? count
                : ""}
            </Box>
            <Box aria-hidden sx={{ alignItems: "center", color: ds.gray["600"], display: { xxs: "none", md: "flex" }, fontSize: 12, gap: 0.75 }}>
              {t("globalSearch.open")} <Kbd>Enter</Kbd>
              <Box component="span" sx={{ ml: 1 }}>{t("globalSearch.closeKey")}</Box> <Kbd>Esc</Kbd>
            </Box>
          </Box>
        </>
      ) : (
        renderBrowse()
      )}
    </Box>
  );
};

/** The header's "Find anything in GovTool" dialog. */
export const GlobalSearchDialog = ({ open, onClose }: { open: boolean; onClose: () => void }) => (
  <Dialog
    aria-labelledby="global-search-title"
    maxWidth={false}
    onClose={onClose}
    open={open}
    PaperProps={
      {
        "data-testid": "global-search-dialog",
        sx: {
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: `${RADIUS.xl}px`,
          boxSizing: "border-box",
          boxShadow: "0px 24px 48px -12px rgba(16, 24, 40, 0.18)",
          m: { xxs: 2, md: 4 },
          maxHeight: { xxs: "calc(100% - 32px)", md: "calc(100% - 96px)" },
          mt: { xxs: 2, md: 9.5 },
          p: { xxs: 2.5, md: 4 },
          width: 980,
        },
      } as object
    }
    sx={{
      "& .MuiBackdrop-root": { backgroundColor: "rgba(0, 0, 0, 0.56)" },
      "& .MuiDialog-container": { alignItems: "flex-start" },
    }}
  >
    <SearchPanel onClose={onClose} />
  </Dialog>
);
