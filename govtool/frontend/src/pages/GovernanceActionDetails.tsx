import { useEffect } from "react";
import {
  useNavigate,
  useLocation,
  useParams,
  generatePath,
} from "react-router-dom";
import { Box, CircularProgress, Link } from "@mui/material";

import { Background, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useGetProposalQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { Footer, TopNav, GovernanceActionDetailsCard } from "@organisms";
import {
  formatDisplayDate,
  getProposalTypeLabel,
  WALLET_LS_KEY,
  getItemFromLocalStorage,
  getShortenedGovActionId,
} from "@utils";
import { Breadcrumbs } from "@molecules";

// TODO: Remove when data validation is ready
const isDataMissing = false;

export const GovernanceActionDetails = () => {
  const { state, hash } = useLocation();
  const navigate = useNavigate();
  const { pagePadding, isMobile } = useScreenDimension();
  const { isEnabled } = useCardano();
  const { t } = useTranslation();
  const { proposalId } = useParams();
  const fullProposalId = proposalId + hash;

  const { data, isLoading } = useGetProposalQuery(fullProposalId ?? "", !state);

  const shortenedGovActionId = getShortenedGovActionId(
    state ? state.txHash : data?.proposal.txHash ?? "",
    state ? state.index : data?.proposal.index ?? "",
  );
  const title = state ? state.title : data.proposal.title;

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      const { pathname } = window.location;
      navigate(`/connected${pathname}`);
    }
  }, [isEnabled]);

  return (
    <Background opacity={0.7}>
      <Box
        display="flex"
        flexDirection="column"
        justifyContent="flex-start"
        minHeight="100vh"
      >
        <TopNav />
        <Box
          display="flex"
          flex={1}
          flexDirection="row"
          mt={2}
          px={pagePadding}
        >
          <Box display="flex" flex={1} flexDirection="column" width="100%">
            {isMobile ? (
              <Box
                sx={{
                  display: "flex",
                  alignItems: "center",
                  padding: "8px 0 24px",
                  mb: "10px",
                  borderBottom: "1px solid #FFF",
                }}
              >
                <Typography variant="title1">
                  {t("govActions.title")}
                </Typography>
              </Box>
            ) : null}
            <Breadcrumbs
              elementOne={t("govActions.title")}
              elementOnePath={PATHS.dashboardGovernanceActions}
              // TODO: Remove "Fund our project" when title is implemented everywhere
              elementTwo={title ?? "Fund our project"}
              isDataMissing={false}
            />
            <Link
              sx={{
                cursor: "pointer",
                display: "flex",
                textDecoration: "none",
              }}
              onClick={() =>
                navigate(
                  state && state.openedFromCategoryPage
                    ? generatePath(PATHS.governanceActionsCategory, {
                        category: state.type,
                      })
                    : PATHS.governanceActions,
                )
              }
            >
              <img
                alt="arrow"
                src={ICONS.arrowRightIcon}
                style={{ marginRight: "12px", transform: "rotate(180deg)" }}
              />
              <Typography color="primary" fontWeight={400} variant="body2">
                {t("back")}
              </Typography>
            </Link>
            {isLoading ? (
              <Box
                alignItems="center"
                display="flex"
                flex={1}
                justifyContent="center"
              >
                <CircularProgress />
              </Box>
            ) : data || state ? (
              <Box data-testid="governance-action-details">
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
                  motivation={
                    state ? state.motivation : data.proposal.motivation
                  }
                  rationale={state ? state.rationale : data.proposal.rationale}
                  yesVotes={state ? state.yesVotes : data.proposal.yesVotes}
                  govActionId={fullProposalId}
                />
              </Box>
            ) : (
              <Box display="flex" flexWrap="wrap" mt={4}>
                <Typography fontWeight={300}>
                  {t("govActions.withIdNotExist.partOne")}
                  &nbsp;
                </Typography>
                <Typography fontWeight={500}>
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
        <Footer />
      </Box>
    </Background>
  );
};
