import React, { FC, useEffect, useState } from "react";
import { Trans, useTranslation } from "react-i18next";
import { useNavigate } from "react-router";
import { Box, ButtonBase, CircularProgress } from "@mui/material";
import { UserRoundPlus, X } from "lucide-react";

import { Button, Typography } from "@atoms";
import { DREP_DIRECTORY_FILTERS, PATHS } from "@consts";
import {
  useCardano,
  useDataActionsBar,
  useFeatureFlag,
  useModal,
  usePagination,
} from "@context";
import {
  useDelegateTodRep,
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useGetDRepDetailsQuery,
  useGetDRepListPaginatedQuery,
} from "@hooks";
import {
  EmptyStateDrepDirectory,
  PageTitle,
  SearchField,
  SearchNotReady,
  SortSelect,
} from "@molecules";
import {
  AutomatedVotingOptions,
  DelegationReviewDialog,
  DRepBanner,
  DRepCard,
} from "@organisms";
import {
  isSameDRep,
  uniqBy,
  parseBoolean,
  correctDRepDirectoryFormat,
} from "@utils";
import { DRepData, DRepListSort, DRepStatus } from "@models";
import {
  AutomatedVotingOptionCurrentDelegation,
  AutomatedVotingOptionDelegationId,
} from "@/types/automatedVotingOptions";
import usePrevious from "@/hooks/usePrevious";
import { PaginationFooter } from "@/components/molecules/PaginationFooter";
import { useUpdateEffect } from "@/hooks/useUpdateEffect";
import { ds, RADIUS } from "@/consts/designTokens";

interface DRepDirectoryContentProps {
  isConnected?: boolean;
}

const Loader = () => (
  <Box
    sx={{
      display: "flex",
      flex: 1,
      height: "100%",
      alignItems: "center",
      justifyContent: "center",
    }}
  >
    <CircularProgress />
  </Box>
);

export const DRepDirectoryContent: FC<DRepDirectoryContentProps> = ({
  isConnected,
}) => {
  const { dRepID: myDRepId, pendingTransaction, stakeKey } = useCardano();
  const { dRepDirectorySort } = useFeatureFlag();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { openModal } = useModal();

  const {
    searchText,
    debouncedSearchText,
    setSearchText,
    lastPath,
    ...dataActionsBarProps
  } = useDataActionsBar();

  const SEED_STORAGE_KEY = "drep_directory_sorting_seed";

  const makeSeed = () =>
      (globalThis.crypto?.randomUUID?.() as string | undefined) ??
      Math.random().toString(36).slice(2);

  const getStoredSeed = () => {
    if (typeof window === "undefined") return "";
    return sessionStorage.getItem(SEED_STORAGE_KEY) || "";
  };

  const [sortingSeed, setSortingSeed] = useState<string>(() => getStoredSeed());

  useEffect(() => {
    if (lastPath && !lastPath.includes("/dreps")) {
      const newSeed = makeSeed();
      setSortingSeed(newSeed);
      sessionStorage.setItem(SEED_STORAGE_KEY, newSeed);
    }
  }, [sortingSeed]);

  const { page, pageSize, setPage, setPageSize } = usePagination();

  const { chosenFilters, chosenSorting, setChosenFilters, setChosenSorting } =
    dataActionsBarProps;

  const [inProgressDelegationDRepData, setInProgressDelegationDRepData] =
    useState<DRepData | undefined>(undefined);
  // The DRep whose delegation is being reviewed, before and while it is signed.
  const [reviewDRep, setReviewDRep] = useState<DRepData | undefined>();

  useEffect(() => {
    if (!lastPath.includes("/dreps")) {
      setChosenFilters([DRepStatus.Active]);
      setSearchText("");
    }
  }, []);

  // GovTool forces `Activity` on mount and keeps it across navigation, so it is
  // the selection most likely to be gated away: Koios directory rows omit
  // activity. Pick an allowed fallback, or clear the selection if no ordering
  // is available, rather than sending a key the backend cannot honour.
  useEffect(() => {
    if (!dRepDirectorySort.isAvailable) {
      if (chosenSorting) setChosenSorting("");
      return;
    }
    if (
      !chosenSorting ||
      dRepDirectorySort.isSelectionStale(chosenSorting)
    ) {
      setChosenSorting(
        dRepDirectorySort.fallbackSelection(DRepListSort.Activity) ?? "",
      );
    }
  }, [chosenSorting, setChosenSorting, dRepDirectorySort]);

  useUpdateEffect(() => {
    setPage(1);
  }, [debouncedSearchText, chosenSorting, JSON.stringify(chosenFilters)]);

  const { delegate, isDelegating } = useDelegateTodRep();

  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const inProgressDelegation = pendingTransaction.delegate?.resourceId;
  const prevInProgressDelegation = usePrevious(inProgressDelegation);

  const { dRep: myDrep } = useGetDRepDetailsQuery(currentDelegation?.dRepView, {
    enabled: !!inProgressDelegation || !!currentDelegation,
  });

  const {
    dRepData: dRepList,
    isFetching,
    isPreviousData: isPrev,
    total,
    baselineTotalForStatus,
    isSearchNotReady,
  } = useGetDRepListPaginatedQuery(
    {
      page: page - 1,
      pageSize,
      searchPhrase: debouncedSearchText,
      sorting: chosenSorting as DRepListSort,
      status: chosenFilters as DRepStatus[],
      sortingSeed,
      listed: true,
    },
    { enabled: !!chosenSorting },
  );

  const showSearchSummary =
    !isSearchNotReady &&
    searchText !== "" &&
    (!isFetching || !isPrev) &&
    total !== baselineTotalForStatus;

  useEffect(() => {
    if (!inProgressDelegation && prevInProgressDelegation) {
      setInProgressDelegationDRepData(undefined);
    }
  }, [prevInProgressDelegation, inProgressDelegation]);

  // The dialog follows a delegation across the loader that shows while the
  // delegation query refetches after signing, so it sits at the same place
  // in both returns and keeps its step.
  const reviewDialog = (
    <DelegationReviewDialog
      dRep={reviewDRep}
      onClose={() => setReviewDRep(undefined)}
      onConfirm={async () => {
        if (!reviewDRep) return;
        setInProgressDelegationDRepData(reviewDRep);
        await delegate(reviewDRep.drepId);
      }}
      onViewDRep={() =>
        reviewDRep &&
        navigate(
          PATHS.dRepDirectoryDRep.replace(":dRepId", reviewDRep.view),
        )
      }
    />
  );

  if (
    (stakeKey && votingPower === undefined) ||
    (!dRepList && !isSearchNotReady) ||
    (isConnected && currentDelegation === undefined)
  ) {
    return (
      <>
        <Loader />
        {reviewDialog}
      </>
    );
  }

  const ada = correctDRepDirectoryFormat(votingPower);

  const filteredDoNotListDReps = uniqBy(
    (dRepList ?? []).filter((dRep) => {
      if (typeof dRep.doNotList === "string") {
        return !parseBoolean(dRep.doNotList);
      }
      return !dRep.doNotList;
    }),
    "view",
  );

  const isAnAutomatedVotingOptionChosen =
    currentDelegation?.dRepView &&
    (currentDelegation?.dRepView ===
      AutomatedVotingOptionCurrentDelegation.drep_always_abstain ||
      currentDelegation?.dRepView ===
        AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence);

  const statusFilter = chosenFilters.length === 1 ? chosenFilters[0] : "";
  const activeFilterCount =
    (chosenFilters.length ? 1 : 0) + (debouncedSearchText ? 1 : 0);
  const clearAll = () => {
    setChosenFilters([]);
    setSearchText("");
  };
  // Only the DRep actually pending: a rejected attempt leaves the data behind.
  const showInProgressCard =
    !!inProgressDelegation &&
    inProgressDelegation !== myDRepId &&
    !!inProgressDelegationDRepData &&
    isSameDRep(inProgressDelegationDRepData, inProgressDelegation);
  const showMyDelegation =
    !!myDrep &&
    !inProgressDelegation &&
    !!currentDelegation &&
    currentDelegation?.dRepHash !== myDRepId;

  return (
    <>
      <Box
        display="flex"
        flex={1}
        flexDirection="column"
        gap={3}
        sx={{ width: "100%" }}
      >
        <PageTitle
          action={
            <Button
              dataTestId="become-a-drep-button"
              onClick={() =>
              (isConnected
                ? navigate(PATHS.registerAsdRep)
                : openModal({ type: "chooseWallet" }))
            }
              color="secondary"
              size="large"
              startIcon={<UserRoundPlus aria-hidden size={18} />}
            >
              {t("dRepDirectory.becomeADRep")}
            </Button>
        }
          description={t("dRepDirectory.pageDescription")}
          title={t("dRepDirectory.pageTitle")}
        />

        {(showMyDelegation || showInProgressCard) && (
        <Box component="section" sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
          <Typography component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600 }}>
            {showMyDelegation ? (
              <Trans i18nKey="dRepDirectory.myDelegation" values={{ ada }} />
            ) : (
              t("dRepDirectory.delegationInProgress")
            )}
          </Typography>
          <Box sx={gridSx}>
            {showMyDelegation && myDrep && (
            <DRepCard
              dRep={myDrep}
              isConnected={!!isConnected}
              isInProgress={isSameDRep(myDrep, inProgressDelegation)}
              isMe={isSameDRep(myDrep, myDRepId)}
              isMyDrep
            />
            )}
            {!showMyDelegation && showInProgressCard && inProgressDelegationDRepData && (
            <DRepCard
              dRep={inProgressDelegationDRepData}
              isConnected={!!isConnected}
              isMe={isSameDRep(inProgressDelegationDRepData, myDRepId)}
              isInProgress
            />
            )}
          </Box>
        </Box>
      )}

        {isConnected && (
        <Box component="section">
          <Typography component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, mb: 1.5 }}>
            {t("dRepDirectory.delegationOptions")}
          </Typography>
          <AutomatedVotingOptions
            currentDelegation={
              !pendingTransaction.delegate && isAnAutomatedVotingOptionChosen
                ? currentDelegation?.dRepView
                : undefined
            }
            delegate={delegate}
            delegationInProgress={
              inProgressDelegation &&
              (inProgressDelegation ===
                AutomatedVotingOptionDelegationId.abstain ||
                inProgressDelegation ===
                  AutomatedVotingOptionDelegationId.no_confidence)
                ? inProgressDelegation
                : undefined
            }
            isConnected={!!isConnected}
            isDelegationLoading={isDelegating}
            votingPower={ada.toString()}
            pendingTransaction={pendingTransaction}
            txHash={
              !pendingTransaction.delegate && isAnAutomatedVotingOptionChosen
                ? currentDelegation?.txHash
                : undefined
            }
          />
        </Box>
      )}

        <Box component="section" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
          {isConnected && (
          <Typography component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600 }}>
            {t("dRepDirectory.listTitle")}
          </Typography>
        )}
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: { xxs: "wrap", md: "nowrap" }, gap: 1.5 }}>
            <SearchField
              onChange={setSearchText}
              onClear={() => setSearchText("")}
              placeholder={t("dRepDirectory.searchBarPlaceholder")}
              value={searchText}
            />
            <SortSelect
              dataTestId="status-filter-button"
              label={t("dRepDirectory.statusLabel")}
              onChange={(next) => setChosenFilters(next ? [next] : [])}
              options={[
              { key: "", label: t("dRepDirectory.statusAll") },
              ...DREP_DIRECTORY_FILTERS,
            ]}
              value={statusFilter}
            />
            {dRepDirectorySort.isAvailable && (
            <SortSelect
              label={t("dRepDirectory.sortLabel")}
              onChange={setChosenSorting}
              options={dRepDirectorySort.options}
              value={chosenSorting}
            />
          )}
          </Box>

          {activeFilterCount > 0 && (
          <Box
            data-testid="active-filters"
            sx={{
              alignItems: "center",
              backgroundColor: ds.primary["25"],
              border: `1px solid ${ds.primary["100"]}`,
              borderRadius: `${RADIUS.md}px`,
              display: "flex",
              flexWrap: "wrap",
              gap: 1,
              px: 1.5,
              py: 1,
            }}
          >
            <Box component="span" sx={{ alignItems: "center", color: ds.gray["700"], display: "inline-flex", fontSize: 13, fontWeight: 500, gap: 0.75 }}>
              {t("dRepDirectory.activeFilters")}
              <Box component="span" sx={{ backgroundColor: ds.primary["600"], borderRadius: "50%", color: "#FFFFFF", fontSize: 11, fontWeight: 600, height: 18, lineHeight: "18px", textAlign: "center", width: 18 }}>
                {activeFilterCount}
              </Box>
            </Box>
            {statusFilter && (
              <FilterChip
                dataTestId={`${statusFilter}-filter-chip`}
                label={statusFilter}
                onRemove={() => setChosenFilters([])}
                removeLabel={t("dRepDirectory.removeFilter", { name: statusFilter })}
              />
            )}
            {chosenFilters.length > 1 && (
              <FilterChip
                dataTestId="status-filter-chip"
                label={chosenFilters.join(", ")}
                onRemove={() => setChosenFilters([])}
                removeLabel={t("dRepDirectory.removeFilter", { name: chosenFilters.join(", ") })}
              />
            )}
            {debouncedSearchText && (
              <FilterChip
                dataTestId="search-filter-chip"
                label={`"${debouncedSearchText}"`}
                onRemove={() => setSearchText("")}
                removeLabel={t("dRepDirectory.removeFilter", { name: debouncedSearchText })}
              />
            )}
            <ButtonBase
              data-testid="clear-all-filters-button"
              onClick={clearAll}
              sx={{ color: ds.error["600"], fontFamily: "inherit", fontSize: 13, fontWeight: 600, px: 0.5 }}
            >
              {t("dRepDirectory.clearAll")}
            </ButtonBase>
          </Box>
        )}

          {showSearchSummary && (
          <Typography sx={{ color: ds.gray["600"], fontSize: 14 }}>
            <Trans
              i18nKey="dRepDirectory.searchSummary"
              defaults="Found {{found}} DRep{{multiple}} out of a total of <total>{{total}}</total>"
              values={{
                found: total ?? "",
                multiple: total && total > 1 ? "s" : "",
                total: baselineTotalForStatus ?? "",
              }}
              components={{
                total:
                  baselineTotalForStatus === undefined ? (
                    <CircularProgress
                      size={16}
                      sx={{ mx: 0.5, verticalAlign: "middle" }}
                    />
                  ) : (
                    <React.Fragment />
                  ),
              }}
            />
          </Typography>
        )}

          {isSearchNotReady && <SearchNotReady />}
          {!isSearchNotReady && filteredDoNotListDReps.length === 0 && (
          <EmptyStateDrepDirectory
            onBrowseAll={clearAll}
            onClearSearch={() => setSearchText("")}
            searchTerm={debouncedSearchText}
          />
        )}
          {!isSearchNotReady && filteredDoNotListDReps.length > 0 && (
          <Box component="ul" sx={{ ...gridSx, m: 0, p: 0 }}>
            {filteredDoNotListDReps.map((dRep) => (
              <Box key={dRep.view} component="li" sx={{ display: "flex", listStyle: "none", minWidth: 0 }}>
                <DRepCard
                  dRep={dRep}
                  isConnected={!!isConnected}
                  isDelegationLoading={
                    isDelegating === dRep.view || isDelegating === dRep.drepId
                  }
                  isDelegateDisabled={!!isDelegating || !!pendingTransaction.delegate}
                  isMe={isSameDRep(dRep, myDRepId)}
                  isMyDrep={isSameDRep(dRep, currentDelegation?.dRepView)}
                  onDelegate={() => setReviewDRep(dRep)}
                />
              </Box>
            ))}
          </Box>
        )}

          <PaginationFooter
          // Mainnet lists about a thousand DReps; keep the page size choice.
            showPageSize
            pageSizeOptions={[6, 12, 24, 48]}
            page={page}
            total={total || 0}
            pageSize={pageSize}
            onPageChange={setPage}
            onPageSizeChange={(n) => {
            setPageSize(n);
            setPage(1);
          }}
          />
        </Box>

        <Box sx={{ mt: 3 }}>
          <DRepBanner withGutters={false} />
        </Box>
      </Box>
      {reviewDialog}
    </>
  );
};

const gridSx = {
  display: "grid",
  gap: 2.5,
  gridTemplateColumns: {
    xxs: "minmax(0, 1fr)",
    md: "repeat(2, minmax(0, 1fr))",
    lg: "repeat(3, minmax(0, 1fr))",
  },
};

const FilterChip = ({
  label,
  onRemove,
  removeLabel,
  dataTestId,
}: {
  label: string;
  onRemove: () => void;
  removeLabel: string;
  dataTestId: string;
}) => (
  <Box
    component="span"
    data-testid={dataTestId}
    sx={{
      alignItems: "center",
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.pill}px`,
      color: ds.gray["900"],
      display: "inline-flex",
      fontSize: 13,
      gap: 0.5,
      lineHeight: "18px",
      maxWidth: "100%",
      minWidth: 0,
      pl: 1.25,
      pr: 0.5,
      py: 0.25,
    }}
  >
    <Box component="span" sx={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
      {label}
    </Box>
    <ButtonBase
      aria-label={removeLabel}
      onClick={onRemove}
      sx={{ borderRadius: "50%", color: ds.gray["600"], height: 20, width: 20 }}
    >
      <X aria-hidden size={12} />
    </ButtonBase>
  </Box>
);
