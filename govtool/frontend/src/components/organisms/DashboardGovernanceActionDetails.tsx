import { useEffect, useRef, useState } from "react";
import {
  useNavigate,
  useLocation,
  useParams,
  generatePath,
} from "react-router-dom";
import { Box, CircularProgress, Link, Typography } from "@mui/material";
import { AxiosError } from "axios";

import { ICONS, OUTCOMES_PATHS, PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useGetProposalQuery,
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { getFullGovActionId, getShortenedGovActionId } from "@utils";
import { GovernanceActionDetailsCard } from "@organisms";
import { Breadcrumbs } from "@molecules";
import { MetadataStandard, ProposalData, ProposalVote } from "@/models";
import { useValidateMutation } from "@/hooks/mutations";

type DashboardGovernanceActionDetailsState = {
  proposal?: ProposalData;
  vote?: ProposalVote;
  openedFromCategoryPage?: boolean;
};

// TODO: Refactor: GovernanceActionDetals and DashboardGovernanceActionDetails are almost identical
// and should be unified
export const DashboardGovernanceActionDetails = () => {
  const { voter } = useGetVoterInfo();
  const { pendingTransaction, isEnableLoading } = useCardano();
  const { state: untypedState, hash } = useLocation();
  const state = untypedState as DashboardGovernanceActionDetailsState | null;
  const index = hash.slice(1);
  const navigate = useNavigate();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { proposalId: txHash } = useParams();

  const fullProposalId = txHash && getFullGovActionId(txHash, +index);
  const shortenedGovActionId =
    txHash && getShortenedGovActionId(txHash, +index);

  const { data, isLoading, error } = useGetProposalQuery(
    fullProposalId ?? "",
    !state?.proposal || !state?.vote,
  );
  const proposal = (data ?? state)?.proposal;
  const vote = (data ?? state)?.vote;

  const [isValidating, setIsValidating] = useState(false);
  const metadataStatus = useRef<MetadataValidationStatus | undefined>();
  const { validateMetadata } = useValidateMutation();

  useEffect(() => {
    const validate = async () => {
      setIsValidating(true);

      const { status } = await validateMetadata({
        standard: MetadataStandard.CIP108,
        url: proposal?.url ?? "",
        hash: proposal?.metadataHash ?? "",
      });

      metadataStatus.current = status;
      setIsValidating(false);
    };
    validate();
  }, []);

  useEffect(() => {
    const isProposalNotFound =
      (error as AxiosError)?.response?.data ===
      `Proposal with id: ${fullProposalId} not found`;
    if (isProposalNotFound && fullProposalId) {
      navigate(
        OUTCOMES_PATHS.governanceActionOutcomes.replace(":id", fullProposalId),
      );
    }
  }, [error]);

  return (
    <Box
      px={isMobile ? 2 : 4}
      pb={3}
      pt={1.25}
      display="flex"
      flexDirection="column"
      flex={1}
    >
      <Breadcrumbs
        elementOne={t("govActions.title")}
        elementOnePath={PATHS.dashboardGovernanceActions}
        elementTwo={proposal?.title ?? ""}
        isDataMissing={metadataStatus?.current ?? null}
      />
      <Link
        data-testid="back-to-list-link"
        sx={{
          cursor: "pointer",
          display: "flex",
          textDecoration: "none",
        }}
        onClick={() =>
          navigate(
            state?.openedFromCategoryPage
              ? generatePath(PATHS.dashboardGovernanceActionsCategory, {
                  category: state?.proposal?.type,
                })
              : PATHS.dashboardGovernanceActions,
            {
              state: {
                isVotedListOnLoad: !!vote,
              },
            },
          )
        }
      >
        <img
          src={ICONS.arrowRightIcon}
          alt="arrow"
          style={{ marginRight: "12px", transform: "rotate(180deg)" }}
        />
        <Typography variant="body2" color="primary">
          {t("back")}
        </Typography>
      </Link>
      <Box display="flex" flex={1} justifyContent="center">
        {isLoading || isEnableLoading ? (
          <Box
            sx={{
              alignItems: "center",
              display: "flex",
              flex: 1,
              justifyContent: "center",
            }}
          >
            <CircularProgress />
          </Box>
        ) : proposal ? (
          <GovernanceActionDetailsCard
            proposal={proposal}
            vote={vote}
            isVoter={
              voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter
            }
            isDataMissing={metadataStatus?.current}
            isInProgress={
              pendingTransaction.vote?.resourceId ===
              fullProposalId?.replace("#", "")
            }
            isDashboard
            isValidating={isValidating}
          />
        ) : (
          <Box mt={4} display="flex" flexWrap="wrap">
            <Typography fontWeight={300}>
              {t("govActions.withIdNotExist.partOne")}
              &nbsp;
            </Typography>
            <Typography fontWeight="bold">
              {` ${shortenedGovActionId} `}
            </Typography>
            <Typography fontWeight={300}>
              &nbsp;
              {t("govActions.withIdNotExist.partTwo")}
            </Typography>
          </Box>
        )}
      </Box>
    </Box>
  );
};
