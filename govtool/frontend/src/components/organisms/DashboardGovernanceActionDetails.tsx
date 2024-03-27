import {
  useNavigate,
  useLocation,
  useParams,
  generatePath,
} from "react-router-dom";
import { Box, CircularProgress, Link, Typography } from "@mui/material";

import { ICONS, PATHS } from "@consts";
import {
  useGetProposalQuery,
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import {
  formatDisplayDate,
  getShortenedGovActionId,
  getProposalTypeLabel,
} from "@utils";
import { GovernanceActionDetailsCard } from "@organisms";
import { Breadcrumbs } from "@molecules";
import { useCardano } from "@/context";

// TODO: Remove when data validation is ready
const isDataMissing = false;

export const DashboardGovernanceActionDetails = () => {
  const { voter } = useGetVoterInfo();
  const { pendingTransaction } = useCardano();
  const { state, hash } = useLocation();
  const navigate = useNavigate();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { proposalId } = useParams();
  const fullProposalId = proposalId + hash;

  const { data, isLoading } = useGetProposalQuery(fullProposalId ?? "", !state);

  const shortenedGovActionId = getShortenedGovActionId(
    state ? state.txHash : data?.proposal.txHash ?? "",
    state ? state.index : data?.proposal.index ?? "",
  );
  const title = state ? state.title : data?.proposal.title;

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
        // TODO: Remove "Fund our project" when title is implemented everywhere
        elementTwo={title ?? "Fund our project"}
        isDataMissing={false}
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
            state && state.openedFromCategoryPage
              ? generatePath(PATHS.dashboardGovernanceActionsCategory, {
                  category: state.type,
                })
              : PATHS.dashboardGovernanceActions,
            {
              state: {
                isVotedListOnLoad: !!(state && state.vote),
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
      <Box display="flex" justifyContent="center">
        {isLoading ? (
          <Box
            display="flex"
            flex={1}
            justifyContent="center"
            alignItems="center"
          >
            <CircularProgress />
          </Box>
        ) : data || state ? (
          <GovernanceActionDetailsCard
            abstainVotes={
              state ? state.abstainVotes : data.proposal.abstainVotes
            }
            createdDate={
              state
                ? formatDisplayDate(state.createdDate)
                : formatDisplayDate(data.proposal.createdDate)
            }
            createdEpochNo={
              state ? state.createdEpochNo : data.proposal.createdEpochNo
            }
            // TODO: Add data validation
            isDataMissing={isDataMissing}
            expiryDate={
              state
                ? formatDisplayDate(state.expiryDate)
                : formatDisplayDate(data.proposal.expiryDate)
            }
            expiryEpochNo={
              state ? state.expiryEpochNo : data.proposal.expiryEpochNo
            }
            isVoter={
              voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter
            }
            noVotes={state ? state.noVotes : data.proposal.noVotes}
            type={
              state
                ? getProposalTypeLabel(state.type)
                : getProposalTypeLabel(data.proposal.type)
            }
            details={state ? state.details : data.proposal.details}
            url={state ? state.url : data.proposal.url}
            title={state ? state.title : data.proposal.title}
            about={state ? state.about : data.proposal.about}
            motivation={state ? state.motivation : data.proposal.motivation}
            rationale={state ? state.rationale : data.proposal.rationale}
            yesVotes={state ? state.yesVotes : data.proposal.yesVotes}
            voteFromEP={data?.vote?.vote}
            govActionId={fullProposalId}
            isInProgress={
              pendingTransaction.vote?.resourceId ===
              fullProposalId.replace("#", "")
            }
            isDashboard
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
