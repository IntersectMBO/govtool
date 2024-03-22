import { Box, CircularProgress } from "@mui/material";
import { FC } from "react";
import { AutomatedVotingOptions, DRepCard } from "@organisms";
import { Typography } from "@atoms";
import { Trans, useTranslation } from "react-i18next";
import { useCardano } from "@/context";
import {
  useDelegateTodRep,
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useGetDRepListQuery
} from "@/hooks";
import { correctAdaFormat, formHexToBech32, isSameDRep } from "@/utils";

interface DRepDirectoryContentProps {
  isConnected?: boolean;
}

export const DRepDirectoryContent: FC<DRepDirectoryContentProps> = ({
  isConnected,
}) => {
  const {
    dRepID: myDRepId,
    isEnabled,
    pendingTransaction,
    stakeKey,
  } = useCardano();
  const { t } = useTranslation();

  const { delegate } = useDelegateTodRep();

  const { votingPower } = useGetAdaHolderVotingPowerQuery();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const inProgressDelegation = pendingTransaction.delegate?.resourceId;

  const { data: myDRepList } = useGetDRepListQuery(
    currentDelegation?.startsWith('drep') ? currentDelegation : formHexToBech32(currentDelegation),
    { enabled: !!inProgressDelegation || !!currentDelegation }
  );
  const myDrep = myDRepList?.[0];
  const { data: dRepList } = useGetDRepListQuery();

  if (!isEnabled || votingPower === undefined || !dRepList) {
    return <CircularProgress sx={{ display: 'block', mx: 'auto', mt: 4 }} />;
  }

  const ada = correctAdaFormat(votingPower);

  return (
    <Box display="flex" flexDirection="column" gap={4}>
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

      <div>
        <Typography fontSize={18} fontWeight={500}>
          {t('dRepDirectory.listTitle')}
        </Typography>
        <Box component="ul" p={0} display="flex" flexDirection="column" gap={3}>
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
