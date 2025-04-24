import { useEffect, useState } from "react";
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
  const [isValidating, setIsValidating] = useState(true);
  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const [isMetadataValid, setIsMetadataValid] = useState<boolean | undefined>();
  const fullProposalId = txHash && getFullGovActionId(txHash, +index);
  const shortenedGovActionId =
    txHash && getShortenedGovActionId(txHash, +index);

  const { data, isLoading, error } = useGetProposalQuery(
    fullProposalId ?? "",
    !state?.proposal || !state?.vote,
  );
  // TODO: Refactor this mess with proposals and metadata validation
  // once authors are existing in all CIP-108 metadata
  const [extendedProposal, setExtendedProposal] = useState<ProposalData>(
    (data ?? state)?.proposal as ProposalData,
  );

  useEffect(() => {
    if (data?.proposal && typeof isMetadataValid !== "boolean") {
      setExtendedProposal(data.proposal);
    }
  }, [data?.proposal, isMetadataValid]);
  const vote = (data ?? state)?.vote;

  const { validateMetadata } = useValidateMutation();

  useEffect(() => {
    if (!extendedProposal?.url) return;

    const validate = async () => {
      setIsValidating(true);

      const { status, metadata, valid } = await validateMetadata({
        standard: MetadataStandard.CIP108,
        url: extendedProposal?.url,
        hash: extendedProposal?.metadataHash ?? "",
      });

      if (metadata) {
        setExtendedProposal((prevProposal) => ({
          ...(prevProposal || {}),
          ...(metadata as Pick<
            ProposalData,
            "title" | "abstract" | "motivation" | "rationale"
          >),
        }));
      }

      setMetadataStatus(status);
      setIsValidating(false);
      setIsMetadataValid(valid);
    };
    validate();
  }, [extendedProposal?.url, extendedProposal?.metadataHash]);

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
        elementTwo={extendedProposal?.title ?? ""}
        isDataMissing={metadataStatus ?? null}
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
        ) : extendedProposal ? (
          <GovernanceActionDetailsCard
            proposal={extendedProposal}
            vote={vote}
            isVoter={
              voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter
            }
            isDataMissing={metadataStatus}
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
