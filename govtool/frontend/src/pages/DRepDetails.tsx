import { useMemo } from "react";
import { useLocation, useNavigate, useParams } from "react-router-dom";
import { Box, CircularProgress } from "@mui/material";

import { PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useGetAdaHolderCurrentDelegationQuery,
  useGetDRepListInfiniteQuery,
  useTranslation,
} from "@hooks";
import { EmptyStateDrepDirectory, LinkWithIcon } from "@molecules";
import { isSameDRep } from "@utils";
import { DRepDetailsCard } from "@organisms";

type DRepDetailsProps = {
  isConnected?: boolean;
};

export const DRepDetails = ({ isConnected = false }: DRepDetailsProps) => {
  const { dRepID: myDRepId, pendingTransaction, stakeKey } = useCardano();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const location = useLocation();
  const { dRepId: dRepParam } = useParams();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);

  const displayBackButton = location.state?.enteredFromWithinApp || false;

  const { dRepData, isDRepListLoading } = useGetDRepListInfiniteQuery({
    searchPhrase: dRepParam,
  });
  const dRep = dRepData?.[0];

  const dRepCardVariant = useMemo(() => {
    if (!dRep) return "default";
    if (isSameDRep(dRep, myDRepId)) return "meAsDRep";
    if (isSameDRep(dRep, currentDelegation?.dRepView)) return "myDRep";
    if (isSameDRep(dRep, pendingTransaction.delegate?.resourceId))
      return "myDRepInProgress";
    return "default";
  }, [
    currentDelegation,
    dRep,
    myDRepId,
    pendingTransaction.delegate?.resourceId,
  ]);

  if (isDRepListLoading)
    return (
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

  if (!dRep)
    return (
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          flex: 1,
          justifyContent: "center",
        }}
      >
        <EmptyStateDrepDirectory />
      </Box>
    );

  return (
    <>
      {displayBackButton ? (
        <LinkWithIcon
          data-testid="back-button"
          label={t("back")}
          onClick={() => navigate(-1)}
          sx={{ mb: 2 }}
        />
      ) : (
        <LinkWithIcon
          data-testid="go-to-drep-directory-button"
          label={t("dRepDirectory.goToDRepDirectory")}
          onClick={() =>
            navigate(
              isConnected ? PATHS.dashboardDRepDirectory : PATHS.dRepDirectory,
            )
          }
          sx={{ mb: 2 }}
        />
      )}
      <DRepDetailsCard
        isConnected={isConnected}
        dRepData={dRep}
        variant={dRepCardVariant}
      />
    </>
  );
};
