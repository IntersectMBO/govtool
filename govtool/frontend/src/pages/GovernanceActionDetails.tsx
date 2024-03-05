import { useEffect } from "react";
import {
  useNavigate,
  useLocation,
  useParams,
  NavLink,
  generatePath,
} from "react-router-dom";
import { Box, Breadcrumbs, CircularProgress, Link } from "@mui/material";

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

export const GovernanceActionDetails = () => {
  const { state, hash } = useLocation();
  const navigate = useNavigate();
  const { pagePadding, screenWidth } = useScreenDimension();
  const { isEnabled } = useCardano();
  const { t } = useTranslation();
  const { proposalId } = useParams();
  const fullProposalId = proposalId + hash;

  const { data, isLoading } = useGetProposalQuery(fullProposalId ?? "", !state);

  const shortenedGovActionId = getShortenedGovActionId(
    state ? state.txHash : data?.proposal.txHash ?? "",
    state ? state.index : data?.proposal.index ?? ""
  );

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      const pathname = window.location.pathname;
      navigate("/connected" + pathname);
    }
  }, [isEnabled]);

  const breadcrumbs = [
    <NavLink
      key="1"
      to={PATHS.governanceActions}
      style={{ textDecorationColor: "#0033AD" }}
    >
      <Typography color="primary" fontWeight={300} variant="caption">
        {t("govActions.title")}
      </Typography>
    </NavLink>,
    <Typography fontWeight={500} key="2" variant="caption">
      {t("govActions.voteOnGovActions")}
    </Typography>,
  ];

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
            {screenWidth >= 1024 ? (
              <Typography fontSize={36} fontWeight={400}>
                {t("govActions.title")}
              </Typography>
            ) : null}
            <Breadcrumbs
              separator="|"
              aria-label="breadcrumb"
              sx={{
                marginBottom: 5,
                marginTop: screenWidth < 1024 ? 2.5 : 3.75,
              }}
            >
              {breadcrumbs}
            </Breadcrumbs>
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
                    : PATHS.governanceActions
                )
              }
            >
              <img
                alt="arrow"
                src={ICONS.arrowRightIcon}
                style={{ marginRight: "12px", transform: "rotate(180deg)" }}
              />
              <Typography color="primary" fontWeight={400} variant="body2">
                {t("backToList")}
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
              <Box
                px={screenWidth < 1024 ? 0 : screenWidth < 1440 ? 10 : 24}
                py={3}
                data-testid={"governance-action-details"}
              >
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
                  noVotes={state ? state.noVotes : data.proposal.noVotes}
                  type={
                    state
                      ? getProposalTypeLabel(state.type)
                      : getProposalTypeLabel(data.proposal.type)
                  }
                  url={state ? state.url : data.proposal.url}
                  yesVotes={state ? state.yesVotes : data.proposal.yesVotes}
                  shortenedGovActionId={shortenedGovActionId}
                />
              </Box>
            ) : (
              <Box display="flex" flexWrap="wrap" mt={4}>
                <Typography fontWeight={300}>
                  {t("govActions.withIdNotExist.partOne")}&nbsp;
                </Typography>
                <Typography
                  fontWeight={500}
                >{` ${shortenedGovActionId} `}</Typography>
                <Typography fontWeight={300}>
                  &nbsp;{t("govActions.withIdNotExist.partTwo")}
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
