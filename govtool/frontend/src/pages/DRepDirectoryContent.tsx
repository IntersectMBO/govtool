import { Box, CircularProgress } from "@mui/material";
import { FC } from "react";
import { AutomatedVotingOptions, DRepCard } from "@organisms";
import { Typography } from "@atoms";
import { Trans, useTranslation } from "react-i18next";
import { Card, DataActionsBar } from "@molecules";
import { useCardano } from "@/context";
import {
  useDataActionsBar,
  useDelegateTodRep,
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useGetDRepListQuery
} from "@/hooks";
import { correctAdaFormat, formHexToBech32, isSameDRep } from "@/utils";
import { DREP_DIRECTORY_FILTERS, DREP_DIRECTORY_SORTING } from "@/consts";

interface DRepDirectoryContentProps {
  isConnected?: boolean;
}

export const DRepDirectoryContent: FC<DRepDirectoryContentProps> = ({
  isConnected,
}) => {
  const {
    dRepID: myDRepId,
    pendingTransaction,
    stakeKey,
  } = useCardano();
  const { t } = useTranslation();
  const { debouncedSearchText, ...dataActionsBarProps } = useDataActionsBar();
  const { chosenFilters, chosenSorting } = dataActionsBarProps;

  const { delegate } = useDelegateTodRep();

  const { votingPower } = useGetAdaHolderVotingPowerQuery();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const inProgressDelegation = pendingTransaction.delegate?.resourceId;

  const { data: myDRepList } = useGetDRepListQuery(
    { drepView: currentDelegation?.startsWith('drep')
      ? currentDelegation
      : formHexToBech32(currentDelegation) },
    { enabled: !!inProgressDelegation || !!currentDelegation }
  );
  const myDrep = myDRepList?.[0];
  const { data: dRepList, isPreviousData } = useGetDRepListQuery({
    drepView: debouncedSearchText,
    sort: chosenSorting,
    status: chosenFilters,
  }, {
    keepPreviousData: true,
  });

  if (stakeKey && votingPower === undefined) {
    return <CircularProgress sx={{ display: 'block', mx: 'auto', mt: 4 }} />;
  }

  const ada = correctAdaFormat(votingPower);

  return (
    <Box display="flex" flexDirection="column" gap={4}>
      {/* My delegation */}
      {myDrep && (
        <div>
          <Typography variant="title2" sx={{ mb: 2 }}>
            <Trans i18nKey="dRepDirectory.myDelegation" values={{ ada }} />
          </Typography>
          <DRepCard
            dRep={myDrep}
            isConnected={!!isConnected}
            isInProgress={isSameDRep(myDrep, inProgressDelegation)}
          />
        </div>
      )}

      {/* Automated voting options */}
      {isConnected && (
        <div>
          <Typography variant="title2" sx={{ mb: 2 }}>
            {t("dRepDirectory.delegationOptions")}
          </Typography>
          <AutomatedVotingOptions
            currentDelegation={(!pendingTransaction.delegate
              && ['drep_always_abstain', 'drep_always_no_confidence'].includes(currentDelegation))
                ? currentDelegation
                : undefined
            }
            delegate={delegate}
            delegationInProgress={inProgressDelegation
              && ['abstain', 'no confidence'].includes(inProgressDelegation)
                ? inProgressDelegation
                : undefined
            }
            isConnected={!!isConnected}
            votingPower={ada.toString()}
          />
        </div>
      )}

      {/* DRep list */}
      <div>
        <Typography fontSize={18} fontWeight={500} sx={{ mb: 3 }}>
          {t('dRepDirectory.listTitle')}
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
          sx={{ opacity: isPreviousData ? 0.5 : 1, transition: 'opacity 0.2s' }}
        >
          {dRepList?.length === 0 && (
            <Card
              border
              elevation={0}
              sx={{
              alignItems: 'center',
              display: 'flex',
              flexDirection: 'column',
              gap: 1,
              py: 5,
            }}
            >
              <Typography fontSize={22}>{t('dRepDirectory.noResultsForTheSearchTitle')}</Typography>
              <Typography fontWeight={400}>{t('dRepDirectory.noResultsForTheSearchDescription')}</Typography>
            </Card>
          )}
          {dRepList?.map((dRep) =>
            (isSameDRep(dRep, myDrep?.view) ? null : (
              <Box key={dRep.drepId} component="li" sx={{ listStyle: 'none' }}>
                <DRepCard
                  dRep={dRep}
                  isConnected={!!isConnected}
                  isInProgress={isSameDRep(dRep, inProgressDelegation)}
                  isMe={isSameDRep(dRep, myDRepId)}
                  onDelegate={() => delegate(dRep.drepId)}
                />
              </Box>
            )),
          )}
        </Box>
      </div>
    </Box>
  );
};
