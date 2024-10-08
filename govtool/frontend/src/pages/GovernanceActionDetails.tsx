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
  WALLET_LS_KEY,
  getFullGovActionId,
  getItemFromLocalStorage,
  getShortenedGovActionId,
} from "@utils";
import { Breadcrumbs } from "@molecules";
import { ProposalData } from "@/models";

type GovernanceActionDetailsState = {
  proposal?: ProposalData;
  openedFromCategoryPage?: boolean;
};

// TODO: Refactor: GovernanceActionDetals and DashboardGovernanceActionDetails are almost identical
// and should be unified
export const GovernanceActionDetails = () => {
  const { state: untypedState, hash } = useLocation();
  const state = untypedState as GovernanceActionDetailsState | null;
  const index = hash.slice(1);
  const navigate = useNavigate();
  const { pagePadding, isMobile } = useScreenDimension();
  const { isEnabled } = useCardano();
  const { t } = useTranslation();
  const { proposalId: txHash } = useParams();

  const fullProposalId = txHash && getFullGovActionId(txHash, index);
  const shortenedGovActionId = txHash && getShortenedGovActionId(txHash, index);

  const { data, isLoading } = useGetProposalQuery(
    fullProposalId ?? "",
    !state?.proposal,
  );
  const proposal = (data ?? state)?.proposal;

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
          pt={2}
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
              elementOnePath={PATHS.governanceActions}
              elementTwo={proposal?.title ?? ""}
              isDataMissing={proposal?.metadataStatus ?? null}
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
                        category: state?.proposal?.type,
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
            ) : proposal ? (
              <Box data-testid="governance-action-details">
                <GovernanceActionDetailsCard
                  isDataMissing={proposal.metadataStatus}
                  proposal={proposal}
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
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
