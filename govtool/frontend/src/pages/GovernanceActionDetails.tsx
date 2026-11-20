import { useEffect, useState } from "react";
import { useNavigate, useLocation, useParams } from "react-router";
import { Box, CircularProgress, Link } from "@mui/material";
import { AxiosError } from "axios";

import { Background, Typography } from "@atoms";
import { ICONS, GOV_ACTION_HISTORY_PATHS, PATHS } from "@consts";
import {
  useGetProposalQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { Footer, TopNav, GovernanceActionDetailsCard } from "@organisms";
import {
  getFullGovActionId,
  getGovActionsTypePath,
  getShortenedGovActionId,
  parseGovActionCIP129Id,
} from "@utils";
import { Breadcrumbs } from "@molecules";
import {
  MetadataIssue,
  MetadataStandard,
  MetadataValidationStatus,
  ProposalData,
} from "@/models";
import { useValidateMutation } from "@/hooks/mutations";

type GovernanceActionDetailsState = {
  proposal?: ProposalData;
  openedFromCategoryPage?: boolean;
};

// TODO: Refactor: GovernanceActionDetals and DashboardGovernanceActionDetails are almost identical
// and should be unified
export const GovernanceActionDetails = () => {
  const { state: untypedState } = useLocation();
  const state = untypedState as GovernanceActionDetailsState | null;
  const navigate = useNavigate();
  const { pagePadding, isMobile } = useScreenDimension();
  const { t } = useTranslation();
  // The CIP-129 gov_action1… id.
  const { proposalId = "" } = useParams();
  const parsedId = parseGovActionCIP129Id(proposalId);

  const fullProposalId =
    parsedId && getFullGovActionId(parsedId.txHash, parsedId.index);
  const shortenedGovActionId = parsedId
    ? getShortenedGovActionId(proposalId, "")
    : proposalId;

  // A row opened from a list carries its document, and so its authors, only
  // when the backend held it. Without one the action is read again for it,
  // while the page shows the row it was given.
  const needsDocument = !!state?.proposal && state.proposal.json == null;
  const { data, isLoading, error } = useGetProposalQuery(
    fullProposalId ?? "",
    !!fullProposalId && (!state?.proposal || needsDocument),
  );
  // TODO: Refactor this mess with proposals and metadata validation
  // once authors are existing in all CIP-108 metadata
  const [extendedProposal, setExtendedProposal] = useState<ProposalData>(
    (data ?? state)?.proposal as ProposalData,
  );

  useEffect(() => {
    if (!data?.proposal) return;
    if (needsDocument) {
      // Only the document and its authors: the text on the page is the one
      // already checked against the anchor.
      const { json, authors } = data.proposal;
      setExtendedProposal((prevProposal) => ({
        ...prevProposal,
        json,
        authors,
      }));
    } else {
      setExtendedProposal(data.proposal);
    }
  }, [data?.proposal]);

  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const [metadataIssues, setMetadataIssues] = useState<MetadataIssue[]>();
  const { validateMetadata } = useValidateMutation();
  // Bumped when a retry resolves the metadata, to validate it again.
  const [validationRevision, setValidationRevision] = useState(0);

  useEffect(() => {
    if (!extendedProposal?.url) return;

    const validate = async () => {
      const { status, metadata, issues } = await validateMetadata({
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
      setMetadataIssues(issues);
    };
    validate();
  }, [extendedProposal?.url, validationRevision]);

  useEffect(() => {
    const isProposalNotFound =
      error instanceof AxiosError &&
      error.response?.data.message.match(/Proposal with id: .* not found/);
    if (isProposalNotFound && fullProposalId) {
      navigate(
        GOV_ACTION_HISTORY_PATHS.governanceActionHistoryDetail.replace(
          ":id",
          proposalId,
        ),
      );
    }
  }, [error]);

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
              elementTwo={extendedProposal?.title ?? ""}
              isDataMissing={metadataStatus ?? null}
            />
            <Link
              sx={{
                cursor: "pointer",
                display: "flex",
                textDecoration: "none",
              }}
              onClick={() =>
                navigate(
                  state?.openedFromCategoryPage && state.proposal?.type
                    ? getGovActionsTypePath(state.proposal.type)
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
            {isLoading && !state?.proposal ? (
              <Box
                alignItems="center"
                display="flex"
                flex={1}
                justifyContent="center"
              >
                <CircularProgress />
              </Box>
            ) : extendedProposal ? (
              <Box data-testid="governance-action-details">
                <GovernanceActionDetailsCard
                  isDataMissing={metadataStatus}
                  isDocumentLoading={needsDocument && isLoading}
                  metadataIssues={metadataIssues}
                  onMetadataRecovered={() =>
                    setValidationRevision((value) => value + 1)
                  }
                  proposal={extendedProposal}
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
