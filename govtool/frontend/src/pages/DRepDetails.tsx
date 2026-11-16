import { useLocation, useNavigate, useParams } from "react-router";
import { Box, CircularProgress } from "@mui/material";
import { ChevronLeft } from "lucide-react";

import { PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useGetAdaHolderCurrentDelegationQuery,
  useGetDRepDetailsQuery,
  useTranslation,
} from "@hooks";
import { Breadcrumbs, EmptyStateDrepDirectory } from "@molecules";
import { isSameDRep } from "@utils";
import { DRepDetailsCard } from "@organisms";
import { ds } from "@/consts/designTokens";

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

  const { dRep, isLoading } = useGetDRepDetailsQuery(dRepParam);

  if (isLoading)
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
      <Breadcrumbs
        elementOne={t("dRepDirectory.title")}
        elementOnePath={
          isConnected ? PATHS.dashboardDRepDirectory : PATHS.dRepDirectory
        }
        elementTwo={dRep.givenName || `${dRep.view.slice(0, 12)}…`}
        isDataMissing={null}
      />
      <Box
        component="button"
        data-testid={displayBackButton ? "back-button" : "go-to-drep-directory-button"}
        onClick={() =>
          (displayBackButton
            ? navigate(-1)
            : navigate(
                isConnected ? PATHS.dashboardDRepDirectory : PATHS.dRepDirectory,
              ))
        }
        type="button"
        sx={{
          alignItems: "center",
          alignSelf: "flex-start",
          background: "none",
          border: "none",
          color: ds.gray["600"],
          cursor: "pointer",
          display: "flex",
          fontFamily: "inherit",
          fontSize: 13,
          gap: 0.5,
          mb: 2,
          mt: 1,
          p: 0,
          "&:hover": { color: ds.primary["600"] },
        }}
      >
        <ChevronLeft aria-hidden size={16} />
        {displayBackButton ? t("back") : t("dRepDirectory.goToDRepDirectory")}
      </Box>
      <DRepDetailsCard
        isConnected={isConnected}
        dRepData={dRep}
        isMe={isSameDRep(dRep, myDRepId)}
        isMyDrep={isSameDRep(dRep, currentDelegation?.dRepView)}
        isMyDrepInProgress={isSameDRep(
          dRep,
          pendingTransaction.delegate?.resourceId,
        )}
      />
    </>
  );
};
