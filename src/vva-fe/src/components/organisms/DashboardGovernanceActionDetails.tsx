import {
  useNavigate,
  useLocation,
  NavLink,
  useParams,
  generatePath,
} from "react-router-dom";
import {
  Box,
  Breadcrumbs,
  CircularProgress,
  Link,
  Typography,
} from "@mui/material";

import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import { useGetProposalQuery, useScreenDimension } from "@hooks";
import { GovernanceActionDetailsCard } from "@organisms";
import {
  formatDisplayDate,
  getShortenedGovActionId,
  getProposalTypeLabel,
} from "@utils";

export const DashboardGovernanceActionDetails = () => {
  const { dRep } = useCardano();
  const { state, hash } = useLocation();
  const navigate = useNavigate();
  const { isMobile, screenWidth } = useScreenDimension();
  const { proposalId } = useParams();
  const fullProposalId = proposalId + hash;

  const { data, isLoading } = useGetProposalQuery(fullProposalId ?? "", !state);

  const shortenedGovActionId = getShortenedGovActionId(
    state ? state.txHash : data?.proposal.txHash ?? "",
    state ? state.index : data?.proposal.index ?? ""
  );

  const breadcrumbs = [
    <NavLink
      key="1"
      to={PATHS.dashboard_governance_actions}
      style={{ textDecorationColor: "#0033AD" }}
    >
      <Typography color="primary" fontWeight={300} fontSize={12}>
        Governance Actions
      </Typography>
    </NavLink>,
    <Typography fontSize={12} fontWeight={500} key="2">
      Vote on Governance Action
    </Typography>,
  ];

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
        separator="|"
        aria-label="breadcrumb"
        sx={{
          marginTop: screenWidth < 1024 ? 2.5 : 0,
          marginBottom: 5,
        }}
      >
        {breadcrumbs}
      </Breadcrumbs>
      <Link
        data-testid={"back-to-list-link"}
        sx={{
          cursor: "pointer",
          display: "flex",
          textDecoration: "none",
        }}
        onClick={() =>
          navigate(
            state && state.openedFromCategoryPage
              ? generatePath(PATHS.dashboard_governance_actions_category, {
                  category: state.type,
                })
              : PATHS.dashboard_governance_actions,
            {
              state: {
                isVotedListOnLoad: state && state.vote ? true : false,
              },
            }
          )
        }
      >
        <img
          src={ICONS.arrowRightIcon}
          alt="arrow"
          style={{ marginRight: "12px", transform: "rotate(180deg)" }}
        />
        <Typography variant="body2" color="primary">
          Back to the list
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
            details={state ? state.details : data.proposal.details}
            expiryDate={
              state
                ? formatDisplayDate(state.expiryDate)
                : formatDisplayDate(data.proposal.expiryDate)
            }
            isDRep={dRep?.isRegistered}
            noVotes={state ? state.noVotes : data.proposal.noVotes}
            type={
              state
                ? getProposalTypeLabel(state.type)
                : getProposalTypeLabel(data.proposal.type)
            }
            url={state ? state.url : data.proposal.url}
            yesVotes={state ? state.yesVotes : data.proposal.yesVotes}
            voteFromEP={data?.vote?.vote}
            shortenedGovActionId={shortenedGovActionId}
          />
        ) : (
          <Box mt={4} display="flex" flexWrap="wrap">
            <Typography fontWeight={300}>
              Governnance action with id&nbsp;
            </Typography>
            <Typography
              fontWeight={"bold"}
            >{` ${shortenedGovActionId} `}</Typography>
            <Typography fontWeight={300}>&nbsp;does not exist.</Typography>
          </Box>
        )}
      </Box>
    </Box>
  );
};
