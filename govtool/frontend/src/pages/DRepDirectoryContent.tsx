import { FC } from "react";
import { Trans, useTranslation } from "react-i18next";
import { Box, CircularProgress } from "@mui/material";

import { Button, Typography } from "@atoms";
import { DREP_DIRECTORY_FILTERS, DREP_DIRECTORY_SORTING } from "@consts";
import { useCardano, useDataActionsBar } from "@context";
import {
  useDelegateTodRep,
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useGetDRepListInfiniteQuery,
} from "@hooks";
import { DataActionsBar, EmptyStateDrepDirectory } from "@molecules";
import { AutomatedVotingOptions, DRepCard } from "@organisms";
import { correctAdaFormat, formHexToBech32, isSameDRep } from "@utils";
import { DRepListSort, DRepStatus } from "@models";
import {
  AutomatedVotingOptionCurrentDelegation,
  AutomatedVotingOptionDelegationId,
} from "@/types/automatedVotingOptions";

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
  const { t } = useTranslation();
  const { debouncedSearchText, ...dataActionsBarProps } = useDataActionsBar();
  const { chosenFilters, chosenSorting } = dataActionsBarProps;

  const { delegate, isDelegating } = useDelegateTodRep();

  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const inProgressDelegation = pendingTransaction.delegate?.resourceId;

  const { dRepData: myDRepList } = useGetDRepListInfiniteQuery(
    {
      searchPhrase: currentDelegation?.dRepView?.startsWith("drep")
        ? currentDelegation.dRepView
        : formHexToBech32(currentDelegation?.dRepHash ?? ""),
    },
    { enabled: !!inProgressDelegation || !!currentDelegation },
  );
  const myDrep = myDRepList?.[0];

  const { dRepData: yourselfDRepList } = useGetDRepListInfiniteQuery({
    searchPhrase: myDRepId,
  });

  const yourselfDRep =
    !!isConnected &&
    (debouncedSearchText === myDRepId || debouncedSearchText === "")
      ? yourselfDRepList?.[0]
      : undefined;

  const {
    dRepData: dRepList,
    isPreviousData,
    dRepListHasNextPage,
    dRepListFetchNextPage,
  } = useGetDRepListInfiniteQuery(
    {
      searchPhrase: debouncedSearchText,
      sorting: chosenSorting as DRepListSort,
      status: chosenFilters as DRepStatus[],
    },
    {
      keepPreviousData: true,
    },
  );

  if (
    (stakeKey && votingPower === undefined) ||
    !dRepList ||
    (isConnected && currentDelegation === undefined)
  ) {
    return <Loader />;
  }

  const ada = correctAdaFormat(votingPower);

  const listedDRepsWithoutYourself = dRepList?.filter(
    (dRep) => !dRep.doNotList && !isSameDRep(dRep, myDRepId),
  );
  const dRepListToDisplay = yourselfDRep
    ? [yourselfDRep, ...listedDRepsWithoutYourself]
    : listedDRepsWithoutYourself;

  const inProgressDelegationDRepData = dRepListToDisplay.find(
    (dRep) =>
      dRep.drepId === inProgressDelegation ||
      dRep.view === inProgressDelegation,
  );

  const isAnAutomatedVotingOptionChosen =
    currentDelegation?.dRepView &&
    (currentDelegation?.dRepView ===
      AutomatedVotingOptionCurrentDelegation.drep_always_abstain ||
      currentDelegation?.dRepView ===
        AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence);

  return (
    <Box display="flex" flex={1} flexDirection="column" gap={4}>
      {/* My delegation */}
      {myDrep &&
        !inProgressDelegation &&
        currentDelegation &&
        currentDelegation?.dRepHash !== myDRepId && (
          <div>
            <Typography variant="title2" sx={{ mb: 2 }}>
              <Trans i18nKey="dRepDirectory.myDelegation" values={{ ada }} />
            </Typography>
            <DRepCard
              dRep={myDrep}
              isConnected={!!isConnected}
              isInProgress={isSameDRep(myDrep, inProgressDelegation)}
              isMe={isSameDRep(myDrep, myDRepId)}
            />
          </div>
        )}
      {inProgressDelegation &&
        inProgressDelegation !== myDRepId &&
        inProgressDelegationDRepData && (
          <DRepCard
            dRep={inProgressDelegationDRepData}
            isConnected={!!isConnected}
            isMe={isSameDRep(inProgressDelegationDRepData, myDRepId)}
            isInProgress
          />
        )}

      {/* Automated voting options */}
      {isConnected && (
        <div>
          <Typography variant="title2" sx={{ mb: 2 }}>
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
        </div>
      )}

      {/* DRep list */}
      <>
        <Typography fontSize={18} fontWeight={500} sx={{ mb: 3 }}>
          {t("dRepDirectory.listTitle")}
        </Typography>
        <DataActionsBar
          {...dataActionsBarProps}
          filterOptions={DREP_DIRECTORY_FILTERS}
          filtersTitle={t("dRepDirectory.filterTitle")}
          sortOptions={DREP_DIRECTORY_SORTING}
        />
        <Box
          component="ul"
          display="flex"
          flexDirection="column"
          gap={3}
          mt={4}
          p={0}
          sx={{
            opacity: isPreviousData ? 0.5 : 1,
            transition: "opacity 0.2s",
            flex: 1,
          }}
        >
          {dRepList?.length === 0 && <EmptyStateDrepDirectory />}
          {dRepListToDisplay?.map((dRep) => {
            if (dRep.doNotList) return null;
            return (
              <Box key={dRep.drepId} component="li" sx={{ listStyle: "none" }}>
                <DRepCard
                  dRep={dRep}
                  isConnected={!!isConnected}
                  isDelegationLoading={
                    isDelegating === dRep.view || isDelegating === dRep.drepId
                  }
                  isMe={isSameDRep(dRep, myDRepId)}
                  onDelegate={() => delegate(dRep.drepId)}
                />
              </Box>
            );
          })}
        </Box>
      </>
      {dRepListHasNextPage && dRepList.length >= 10 && (
        <Box sx={{ justifyContent: "center", display: "flex" }}>
          <Button
            data-testid="show-more-button"
            variant="outlined"
            onClick={() => dRepListFetchNextPage()}
          >
            {t("showMore")}
          </Button>
        </Box>
      )}
    </Box>
  );
};
