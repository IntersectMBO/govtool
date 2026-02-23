import { useEffect, useState } from "react";
import {
  useNavigate,
  useLocation,
  useParams,
  generatePath,
} from "react-router-dom";
import { Box, CircularProgress, Link } from "@mui/material";
import { AxiosError } from "axios";

import { Background, Typography } from "@atoms";
import { ICONS, OUTCOMES_PATHS, PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useGetProposalQuery,
  useGetProposalSurveyQuery,
  useGetProposalSurveyTallyQuery,
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
import {
  MetadataStandard,
  MetadataValidationStatus,
  ProposalData,
} from "@/models";
import { useValidateMutation } from "@/hooks/mutations";
import { GovernanceActionType } from "@/types/governanceAction";

type GovernanceActionDetailsState = {
  proposal?: ProposalData;
  openedFromCategoryPage?: boolean;
};

// TODO: Refactor: GovernanceActionDetals and DashboardGovernanceActionDetails are almost identical
// and should be unified
export const GovernanceActionDetails = () => {
  const [weighting, setWeighting] = useState<"CredentialBased" | "StakeBased">(
    "CredentialBased",
  );
  const { state: untypedState, hash } = useLocation();
  const state = untypedState as GovernanceActionDetailsState | null;
  const index = hash.slice(1);
  const navigate = useNavigate();
  const { pagePadding, isMobile } = useScreenDimension();
  const { isEnabled } = useCardano();
  const { t } = useTranslation();
  const { proposalId: txHash } = useParams();

  const fullProposalId = txHash && getFullGovActionId(txHash, +index);
  const shortenedGovActionId = txHash && getShortenedGovActionId(txHash, +index);

  const { data, isLoading, error } = useGetProposalQuery(
    fullProposalId ?? "",
    !state?.proposal,
  );
  // TODO: Refactor this mess with proposals and metadata validation
  // once authors are existing in all CIP-108 metadata
  const [extendedProposal, setExtendedProposal] = useState<ProposalData>(
    (data ?? state)?.proposal as ProposalData,
  );

  useEffect(() => {
    if (data?.proposal) {
      setExtendedProposal(data.proposal);
    }
  }, [data?.proposal]);

  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const { validateMetadata } = useValidateMutation();

  useEffect(() => {
    if (!extendedProposal?.url) return;

    const validate = async () => {
      const { status, metadata } = await validateMetadata({
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
    };
    validate();
  }, [extendedProposal?.url]);

  useEffect(() => {
    const isProposalNotFound =
      error instanceof AxiosError &&
      error.response?.data.message.match(/Proposal with id: .* not found/);
    if (isProposalNotFound && fullProposalId) {
      navigate(
        OUTCOMES_PATHS.governanceActionOutcomes.replace(":id", fullProposalId),
      );
    } else if (
      isEnabled &&
      getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)
    ) {
      const { pathname } = window.location;
      navigate(`/connected${pathname}`);
    }
  }, [isEnabled, error]);

  const shouldFetchSurvey =
    !!fullProposalId &&
    extendedProposal?.type === GovernanceActionType.InfoAction;
  const { data: surveyData, isLoading: isSurveyLoading } =
    useGetProposalSurveyQuery(fullProposalId ?? "", shouldFetchSurvey);
  const shouldFetchSurveyTally =
    !!shouldFetchSurvey &&
    !!surveyData?.linked &&
    !!surveyData?.linkValidation?.valid &&
    !!surveyData?.surveyDetailsValidation?.valid;
  const { data: surveyTallyData, isLoading: isSurveyTallyLoading } =
    useGetProposalSurveyTallyQuery(
      fullProposalId ?? "",
      weighting,
      shouldFetchSurveyTally,
    );

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
                  state?.openedFromCategoryPage
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
            ) : extendedProposal ? (
              <Box data-testid="governance-action-details">
                <GovernanceActionDetailsCard
                  isDataMissing={metadataStatus}
                  proposal={extendedProposal}
                />
                {extendedProposal.type === GovernanceActionType.InfoAction && (
                  <Box
                    sx={{
                      mt: 3,
                      p: 3,
                      borderRadius: "20px",
                      background: "rgba(255,255,255,0.45)",
                      boxShadow: "2px 2px 20px 0px rgba(47, 98, 220, 0.12)",
                    }}
                  >
                    <Typography variant="headline4">Linked Survey</Typography>
                    {isSurveyLoading ? (
                      <Box mt={2}>
                        <CircularProgress size={22} />
                      </Box>
                    ) : !surveyData?.linked ? (
                      <Typography variant="body2" sx={{ mt: 2 }}>
                        No survey linked to this Info Action.
                      </Typography>
                    ) : (
                      <>
                        <Typography variant="body2" sx={{ mt: 2 }}>
                          {surveyData.surveyDetails?.title ?? "Untitled survey"}
                        </Typography>
                        {surveyData.surveyDetails?.description && (
                          <Typography
                            variant="body2"
                            sx={{ mt: 1, color: "neutralGray" }}
                          >
                            {surveyData.surveyDetails.description}
                          </Typography>
                        )}

                        {(!surveyData.linkValidation.valid ||
                          !surveyData.surveyDetailsValidation.valid) && (
                          <Box mt={2}>
                            <Typography variant="body2" sx={{ fontWeight: 600 }}>
                              Survey validation errors
                            </Typography>
                            {[
                              ...(surveyData.linkValidation.errors ?? []),
                              ...(surveyData.surveyDetailsValidation.errors ?? []),
                            ].map((validationError) => (
                              <Typography
                                key={validationError}
                                variant="caption"
                                sx={{ display: "block", mt: 0.5 }}
                              >
                                • {validationError}
                              </Typography>
                            ))}
                          </Box>
                        )}

                        {surveyData.linkValidation.valid &&
                          surveyData.surveyDetailsValidation.valid && (
                            <>
                              <Box
                                sx={{
                                  mt: 2,
                                  display: "flex",
                                  gap: 1,
                                  flexWrap: "wrap",
                                }}
                              >
                                <button
                                  type="button"
                                  onClick={() => setWeighting("CredentialBased")}
                                  style={{
                                    borderRadius: 20,
                                    border: "1px solid #2F62DC",
                                    padding: "6px 12px",
                                    cursor: "pointer",
                                    background:
                                      weighting === "CredentialBased"
                                        ? "#2F62DC"
                                        : "transparent",
                                    color:
                                      weighting === "CredentialBased"
                                        ? "#fff"
                                        : "#2F62DC",
                                  }}
                                >
                                  CredentialBased
                                </button>
                                <button
                                  type="button"
                                  onClick={() => setWeighting("StakeBased")}
                                  style={{
                                    borderRadius: 20,
                                    border: "1px solid #2F62DC",
                                    padding: "6px 12px",
                                    cursor: "pointer",
                                    background:
                                      weighting === "StakeBased"
                                        ? "#2F62DC"
                                        : "transparent",
                                    color:
                                      weighting === "StakeBased"
                                        ? "#fff"
                                        : "#2F62DC",
                                  }}
                                >
                                  StakeBased
                                </button>
                              </Box>

                              {isSurveyTallyLoading ? (
                                <Box mt={2}>
                                  <CircularProgress size={22} />
                                </Box>
                              ) : surveyTallyData ? (
                                <Box mt={2}>
                                  <Typography
                                    variant="caption"
                                    sx={{ display: "block" }}
                                  >
                                    Total responses seen:{" "}
                                    {surveyTallyData.totals.totalSeen}
                                  </Typography>
                                  <Typography
                                    variant="caption"
                                    sx={{ display: "block" }}
                                  >
                                    Valid latest responses:{" "}
                                    {surveyTallyData.totals.valid}
                                  </Typography>
                                  <Typography
                                    variant="caption"
                                    sx={{ display: "block" }}
                                  >
                                    Invalid responses:{" "}
                                    {surveyTallyData.totals.invalid}
                                  </Typography>

                                  <Box mt={2}>
                                    {surveyTallyData.methodResults.map((result) => {
                                      const resultRecord =
                                        result as Record<string, unknown>;
                                      const questionId = String(
                                        resultRecord.questionId ?? "question",
                                      );
                                      const question = String(
                                        resultRecord.question ?? "",
                                      );
                                      const options = Array.isArray(
                                        resultRecord.options,
                                      )
                                        ? (resultRecord.options as string[])
                                        : [];
                                      const optionTotals = Array.isArray(
                                        resultRecord.optionTotals,
                                      )
                                        ? (resultRecord.optionTotals as number[])
                                        : [];
                                      const customValueTotals =
                                        (resultRecord.customValueTotals as
                                          | Record<string, number>
                                          | undefined) ?? {};

                                      return (
                                        <Box
                                          key={questionId}
                                          sx={{
                                            mt: 2,
                                            p: 2,
                                            borderRadius: "12px",
                                            border:
                                              "1px solid rgba(47,98,220,0.2)",
                                          }}
                                        >
                                          <Typography
                                            variant="body2"
                                            sx={{ fontWeight: 600 }}
                                          >
                                            {question}
                                          </Typography>
                                          {options.length > 0 &&
                                            options.map((option, optionIndex) => (
                                              <Typography
                                                key={`${questionId}-${option}`}
                                                variant="caption"
                                                sx={{ display: "block", mt: 0.5 }}
                                              >
                                                {option}:{" "}
                                                {optionTotals[optionIndex] ?? 0}
                                              </Typography>
                                            ))}
                                          {resultRecord.mean !== undefined && (
                                            <>
                                              <Typography
                                                variant="caption"
                                                sx={{ display: "block", mt: 0.5 }}
                                              >
                                                Count:{" "}
                                                {String(resultRecord.count ?? 0)}
                                              </Typography>
                                              <Typography
                                                variant="caption"
                                                sx={{ display: "block", mt: 0.5 }}
                                              >
                                                Min: {String(resultRecord.min ?? "-")}
                                              </Typography>
                                              <Typography
                                                variant="caption"
                                                sx={{ display: "block", mt: 0.5 }}
                                              >
                                                Max: {String(resultRecord.max ?? "-")}
                                              </Typography>
                                              <Typography
                                                variant="caption"
                                                sx={{ display: "block", mt: 0.5 }}
                                              >
                                                Mean:{" "}
                                                {String(resultRecord.mean ?? "-")}
                                              </Typography>
                                            </>
                                          )}
                                          {Object.keys(customValueTotals).length > 0 &&
                                            Object.entries(customValueTotals).map(
                                              ([valueKey, valueTotal]) => (
                                                <Typography
                                                  key={`${questionId}-${valueKey}`}
                                                  variant="caption"
                                                  sx={{ display: "block", mt: 0.5 }}
                                                >
                                                  {valueKey}: {valueTotal}
                                                </Typography>
                                              ),
                                            )}
                                        </Box>
                                      );
                                    })}
                                  </Box>
                                </Box>
                              ) : null}
                            </>
                          )}
                      </>
                    )}
                  </Box>
                )}
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
