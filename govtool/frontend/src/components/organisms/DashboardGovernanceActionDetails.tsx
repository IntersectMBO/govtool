import { useEffect, useState } from "react";
import {
  useNavigate,
  useLocation,
  useParams,
  generatePath,
} from "react-router";
import { Box, CircularProgress, Link, Typography } from "@mui/material";
import { AxiosError } from "axios";

import { ICONS, GOV_ACTION_HISTORY_PATHS, PATHS } from "@consts";
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
import {
  MetadataIssue,
  MetadataStandard,
  MetadataValidationStatus,
  ProposalData,
  ProposalVote,
} from "@/models";
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
  const [metadataIssues, setMetadataIssues] = useState<MetadataIssue[]>();
  const fullProposalId = txHash && getFullGovActionId(txHash, +index);
  const shortenedGovActionId =
    txHash && getShortenedGovActionId(txHash, +index);

  const needsProposal = !state?.proposal || !state?.vote;
  // A row opened from a list carries its document, and so its authors, only
  // when the backend held it. Without one the action is read again for it,
  // while the page shows the row it was given.
  const needsDocument = !needsProposal && state?.proposal?.json == null;
  const { data, isLoading, error } = useGetProposalQuery(
    fullProposalId ?? "",
    needsProposal || needsDocument,
  );
  // TODO: Refactor this mess with proposals and metadata validation
  // once authors are existing in all CIP-108 metadata
  const [extendedProposal, setExtendedProposal] = useState<ProposalData>(
    (data ?? state)?.proposal as ProposalData,
  );

  useEffect(() => {
    if (!data?.proposal) return;
    const extendedProposalIndex = extendedProposal ? extendedProposal.index : -1;
    if (data.proposal.index !== extendedProposalIndex) {
      setExtendedProposal(data.proposal);
    } else if (needsDocument) {
      // Only the document and its authors: the text on the page is the one
      // already checked against the anchor.
      const { json, authors } = data.proposal;
      setExtendedProposal((prevProposal) => ({
        ...prevProposal,
        json,
        authors,
      }));
    }
  }, [data?.proposal, isMetadataValid]);
  // A row's own vote, which a read made only for its document must not
  // replace: a vote just cast may not have reached the backend yet.
  const vote = (needsProposal ? data ?? state : state)?.vote;

  const { validateMetadata } = useValidateMutation();
  // Bumped when a retry resolves the metadata, to validate it again.
  const [validationRevision, setValidationRevision] = useState(0);

  useEffect(() => {
    if (!extendedProposal?.url) return;

    const validate = async () => {
      setIsValidating(true);

      const { status, metadata, valid, issues } = await validateMetadata({
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
      setIsValidating(false);
      setIsMetadataValid(valid);
    };
    validate();
  }, [
    extendedProposal?.url,
    extendedProposal?.metadataHash,
    validationRevision,
  ]);

  useEffect(() => {
    const isProposalNotFound =
      error instanceof AxiosError &&
      error.response?.data.message.match(/Proposal with id: .* not found/);
    if (isProposalNotFound && fullProposalId) {
      navigate(
        GOV_ACTION_HISTORY_PATHS.governanceActionHistoryDetail.replace(":id", fullProposalId),
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
        {(isLoading && needsProposal) || isEnableLoading ? (
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
            metadataIssues={metadataIssues}
            isInProgress={
              pendingTransaction.vote?.resourceId ===
              fullProposalId?.replace("#", "")
            }
            isDashboard
            isValidating={isValidating}
            isDocumentLoading={needsDocument && isLoading}
            onMetadataRecovered={() =>
              setValidationRevision((value) => value + 1)
            }
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
