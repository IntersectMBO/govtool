import { PropsWithChildren, useEffect, useState } from "react";
import { useLocation, useNavigate } from "react-router";
import { Avatar, Box, ButtonBase, Link, Skeleton } from "@mui/material";
import {
  CircleCheck,
  Copy,
  ExternalLink as ExternalLinkIcon,
  Loader2,
} from "lucide-react";

import { Button, ExternalModalButton, StatusPill, Typography } from "@atoms";
import { PATHS } from "@consts";
import { useCardano, useModal, useSnackbar } from "@context";
import {
  useDelegateTodRep,
  useGetDRepVotesByIdQuery,
  useTranslation,
} from "@hooks";
import {
  DataMissingInfoBox,
  DRepVotingTrend,
  MetadataFailureDetails,
  StatCard,
} from "@molecules";
import {
  correctDRepDirectoryFormat,
  encodeCIP129Identifier,
  formatDisplayDate,
  getBase64ImageDetails,
  getMetadataDataMissingStatusTranslation,
  testIdFromLabel,
} from "@utils";
import { LINKS } from "@/consts/links";
import {
  DRepData,
  DRepStatus,
  MetadataIssue,
  MetadataStandard,
  MetadataValidationStatus,
} from "@/models";
import { useValidateMutation } from "@/hooks/mutations";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";
import { avatarTone, dRepInitials, VOTING_HISTORY_ID } from "./DRepCard";
import { DRepActivityTable } from "./DRepActivityTable";
import { DelegationReviewDialog } from "./DelegationReviewDialog";
import { ShareDRepDialog } from "./DRepDashboard/ShareDRepDialog";

type DRepDetailsProps = {
  dRepData: DRepData;
  isConnected: boolean;
  isMe?: boolean;
  isMyDrep?: boolean;
  isMyDrepInProgress?: boolean;
};

export const DRepDetailsCard = ({
  dRepData,
  isConnected,
  isMe,
  isMyDrep,
  isMyDrepInProgress,
}: DRepDetailsProps) => {
  const { pendingTransaction } = useCardano();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { openModal } = useModal();
  const { delegate, isDelegating } = useDelegateTodRep();
  const [isReviewOpen, setIsReviewOpen] = useState(false);
  const { dRepVotes, areDRepVotesLoading, isDRepVotesError } =
    useGetDRepVotesByIdQuery(dRepData.view);
  const { hash } = useLocation();

  // The directory card's View Voting History lands here; the router does not
  // scroll to a hash, and the table's height settles once the votes load.
  useEffect(() => {
    if (hash !== `#${VOTING_HISTORY_ID}` || areDRepVotesLoading) return;
    document.getElementById(VOTING_HISTORY_ID)?.scrollIntoView({ block: "start" });
  }, [hash, areDRepVotesLoading]);

  const {
    motivations,
    objectives,
    paymentAddress,
    qualifications,
    identityReferences,
    linkReferences,
    status,
    url,
    view,
    drepId,
    votingPower,
    isScriptBased,
    metadataHash,
    latestRegistrationDate,
  } = dRepData;

  const [isValidating, setIsValidating] = useState(false);
  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const [metadataIssues, setMetadataIssues] = useState<MetadataIssue[]>();
  const { validateMetadata } = useValidateMutation();
  // Bumped when a retry resolves the document, to validate it again.
  const [validationRevision, setValidationRevision] = useState(0);

  useEffect(() => {
    if (!url) return;

    const validate = async () => {
      setIsValidating(true);

      const { status: metadataValidationStatus, issues } = await validateMetadata({
        standard: MetadataStandard.CIP119,
        url,
        hash: metadataHash ?? "",
      });

      setMetadataStatus(metadataValidationStatus);
      setMetadataIssues(issues);
      setIsValidating(false);
    };
    validate();
  }, [url, validationRevision]);

  const name =
    dRepData.givenName || `${view.slice(0, 12)}…${view.slice(-6)}`;
  const tone = avatarTone(view);
  const base64Image = getBase64ImageDetails(dRepData.image ?? "");
  const imageSrc = dRepData.image
    ? base64Image.isValidBase64Image
      ? `${base64Image.base64Prefix}${dRepData.image}`
      : dRepData.image
    : undefined;
  const cip129Id = encodeCIP129Identifier({
    txID: `${isScriptBased ? "23" : "22"}${drepId}`,
    bech32Prefix: "drep",
  });
  const canDelegate = ["Active", "Inactive"].includes(status);

  return (
    <Box
      data-testid="drep-details-card"
      sx={{ display: "flex", flexDirection: "column", gap: 3 }}
    >
      {/* HEADER */}
      <Box
        sx={{
          alignItems: { md: "center" },
          display: "flex",
          flexDirection: { xxs: "column", md: "row" },
          gap: 2,
          justifyContent: "space-between",
        }}
      >
        <Box sx={{ alignItems: "center", display: "flex", gap: 2, minWidth: 0 }}>
          {isValidating ? (
            <Skeleton height={64} variant="circular" width={64} />
          ) : (
            <Avatar
              alt=""
              aria-hidden
              data-testid="drep-image"
              src={imageSrc}
              sx={{
                backgroundColor: tone.bg,
                color: tone.text,
                fontSize: 22,
                fontWeight: 600,
                height: 64,
                width: 64,
              }}
            >
              {dRepInitials(dRepData.givenName)}
            </Avatar>
          )}
          <Box sx={{ display: "flex", flexDirection: "column", gap: 0.5, minWidth: 0 }}>
            <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
              {isValidating ? (
                <Skeleton height={36} variant="text" width={200} />
              ) : (
                <Box
                  component="h1"
                  data-testid="drep-name"
                  sx={{
                    color: metadataStatus ? ds.error["600"] : ds.gray["900"],
                    fontSize: { xxs: 24, md: 30 },
                    fontWeight: 600,
                    letterSpacing: "-0.5px",
                    lineHeight: { xxs: "30px", md: "38px" },
                    m: 0,
                    wordBreak: "break-word",
                  }}
                >
                  {metadataStatus ? getMetadataDataMissingStatusTranslation(metadataStatus) : name}
                </Box>
              )}
              <Box data-testid="drep-status">
                <StatusPill status={status} />
              </Box>
              {isMe && <StatusPill status={DRepStatus.Yourself} />}
            </Box>
            <Box sx={{ alignItems: "center", color: ds.gray["500"], display: "flex", flexWrap: "wrap", fontSize: 13, gap: 1 }}>
              <CopyableText dataTestId="header-copy-drep-id-button" isCompact value={cip129Id} />
              {latestRegistrationDate && (
                <>
                  <Box aria-hidden component="span">·</Box>
                  <span>
                    {t("dRepProfile.registeredOn", {
                      date: formatDisplayDate(latestRegistrationDate, "MMM d, yyyy"),
                    })}
                  </span>
                </>
              )}
            </Box>
          </Box>
        </Box>
        <Box sx={{ alignItems: "center", display: "flex", flexShrink: 0, flexWrap: "wrap", gap: 1 }}>
          {isMe && (
            <Button
              data-testid="edit-drep-data-button"
              onClick={() => navigate(PATHS.editDrepMetadata, { state: dRepData })}
              variant="outlined"
            >
              {t("dRepDirectory.editBtn")}
            </Button>
          )}
          {!isValidating && isConnected && canDelegate && !isMyDrep && (
            <Button
              data-testid="delegate-button"
              disabled={!!pendingTransaction?.delegate || !!isDelegating}
              isLoading={
                isDelegating === dRepData.view || isDelegating === dRepData.drepId
              }
              onClick={() => setIsReviewOpen(true)}
              size="large"
            >
              {t("dRepProfile.delegate")}
            </Button>
          )}
          {!isValidating && !isConnected && canDelegate && (
            <Button
              data-testid="connect-to-delegate-button"
              onClick={() =>
                openModal({
                  type: "chooseWallet",
                  state: {
                    pathToNavigate: PATHS.dashboardDRepDirectoryDRep.replace(
                      ":dRepId",
                      view,
                    ),
                  },
                })
              }
              size="large"
              variant="outlined"
            >
              {t("connectToDelegate")}
            </Button>
          )}
          <ShareDRepDialog
            buttonTestId="share-button"
            copyTestId="copy-link-from-share-button"
            dRepId={view}
            link={`${window.location.origin}${window.location.pathname.replace(/^\/connected\//, "/")}`}
          />
        </Box>
      </Box>

      <DelegationReviewDialog
        dRep={isReviewOpen ? dRepData : undefined}
        onClose={() => setIsReviewOpen(false)}
        onConfirm={() => delegate(dRepData.drepId)}
      />

      {/* STATS */}
      <Box
        sx={{
          display: "grid",
          gap: 2,
          gridTemplateColumns: { xxs: "repeat(2, minmax(0, 1fr))", md: "repeat(4, minmax(0, 1fr))" },
        }}
      >
        <StatCard
          dataTestId="drep-voting-power"
          label={t("votingPower")}
          value={<span data-testid="voting-power">{`₳\u00a0${correctDRepDirectoryFormat(votingPower)}`}</span>}
          valueColor={ds.orange["700"]}
        />
        <StatCard
          dataTestId="drep-votes-cast"
          label={t("dRepProfile.votesCast")}
          value={areDRepVotesLoading ? "—" : (dRepVotes?.length ?? "—")}
        />
        <StatCard
          dataTestId="drep-registered"
          label={t("dRepDirectory.card.registered")}
          value={latestRegistrationDate ? formatDisplayDate(latestRegistrationDate, "MMM d, yyyy") : "—"}
        />
        <StatCard
          dataTestId="drep-type"
          label={t("dRepProfile.type")}
          value={isScriptBased ? t("dRepProfile.scriptBased") : t("dRepProfile.keyBased")}
        />
      </Box>

      {/* DELEGATION STATE */}
      {(isMyDrep || isMyDrepInProgress) && (
        <Box
          data-testid={isMyDrepInProgress ? "delegation-in-progress-banner" : "delegated-banner"}
          sx={{
            alignItems: "flex-start",
            backgroundColor: isMyDrepInProgress ? ds.orange["50"] : ds.success["50"],
            border: `1px solid ${isMyDrepInProgress ? ds.orange["200"] : ds.success["200"]}`,
            borderRadius: `${RADIUS.lg}px`,
            display: "flex",
            gap: 1.5,
            p: 2.5,
          }}
        >
          <Box sx={{ color: isMyDrepInProgress ? ds.orange["600"] : ds.success["600"], display: "flex", mt: 0.25 }}>
            {isMyDrepInProgress ? (
              <Loader2 aria-hidden size={20} />
            ) : (
              <CircleCheck aria-hidden size={20} />
            )}
          </Box>
          <Box>
            <Box sx={{ color: isMyDrepInProgress ? ds.orange["700"] : ds.success["700"], fontSize: 14, fontWeight: 600 }}>
              {isMyDrepInProgress
                ? t("dRepProfile.delegationInProgress", { name })
                : t("dRepProfile.delegatedTo", { name })}
            </Box>
            <Box sx={{ color: isMyDrepInProgress ? ds.orange["700"] : ds.success["700"], fontSize: 13, mt: 0.5 }}>
              {t("dRepProfile.delegationNote")}
            </Box>
          </Box>
        </Box>
      )}

      {/* METADATA PROBLEMS */}
      {metadataStatus && (
        <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
          <DataMissingInfoBox
            isDataMissing={metadataStatus}
            issues={metadataIssues}
            isDrep
            sx={{ mb: 0 }}
            isValidating={isValidating}
            reportAction={
              !!url &&
              !!metadataHash && (
                <MetadataFailureDetails
                  anchor={{ url, hash: metadataHash }}
                  canRetry={isMe}
                  onRecovered={() =>
                    setValidationRevision((value) => value + 1)
                  }
                />
              )
            }
          />
          {!!url && (
            <ExternalModalButton
              label={t("govActions.seeExternalData")}
              sx={{ mb: 0, alignSelf: "flex-start" }}
              url={url}
            />
          )}
        </Box>
      )}

      {/* BODY */}
      <Box
        sx={{
          alignItems: "start",
          display: "grid",
          gap: 2.5,
          gridTemplateColumns: { xxs: "minmax(0, 1fr)", md: "320px minmax(0, 1fr)" },
        }}
      >
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
          <Box component="section" sx={panelSx}>
            <PanelTitle>{t("dRepProfile.identity")}</PanelTitle>
            <DRepDetailsInfoItem label={t("drepId")} dataTestId="cip-129-drep-id" isValidating={isValidating}>
              <CopyableText value={cip129Id} dataTestId="copy-cip-129-drep-id-button" />
            </DRepDetailsInfoItem>
            <DRepDetailsInfoItem label={t("cip105DRepId")} dataTestId="cip-105-drep-id" isValidating={isValidating}>
              <CopyableText isSemiTransparent value={view} dataTestId="copy-drep-id-button" />
            </DRepDetailsInfoItem>
            {!metadataStatus && url && (
              <DRepDetailsInfoItem label={t("forms.dRepData.metadataUrl")} dataTestId="metadata-url">
                <ExternalLink dataTestId="metadata-url-link" href={url} />
              </DRepDetailsInfoItem>
            )}
            {!metadataStatus && metadataHash && (
              <DRepDetailsInfoItem label={t("forms.dRepData.metadataHash")} dataTestId="metadata-hash">
                <CopyableText value={metadataHash} dataTestId="copy-metadata-hash" />
              </DRepDetailsInfoItem>
            )}
            {!metadataStatus && paymentAddress && (
              <DRepDetailsInfoItem label={t("forms.dRepData.paymentAddress")} dataTestId="payment-address">
                <CopyableText value={paymentAddress} dataTestId="copy-payment-address-button" />
              </DRepDetailsInfoItem>
            )}
            {!metadataStatus && identityReferences?.length > 0 && (
              <>
                <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}` }} />
                <PanelTitle>{t("forms.dRepData.referenceTypes.identity.title")}</PanelTitle>
                <Box data-testid="references-identity-info-item-description">
                  <ReferencesGroup references={identityReferences} />
                </Box>
              </>
            )}
            {!metadataStatus && linkReferences?.length > 0 && (
              <>
                <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}` }} />
                <PanelTitle>{t("forms.dRepData.referenceTypes.link.title")}</PanelTitle>
                <Box data-testid="references-link-info-item-description">
                  <ReferencesGroup references={linkReferences} />
                </Box>
              </>
            )}
          </Box>

          <Box component="section" sx={panelSx}>
            <PanelTitle>{t("dRepProfile.howDelegationWorks")}</PanelTitle>
            <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>
              {t("dRepProfile.howDelegationWorksBody")}
            </Box>
            <Box
              component="a"
              href={LINKS.DELEGATING}
              rel="noopener noreferrer"
              sx={{ color: ds.primary["600"], fontSize: 14, fontWeight: 600, textDecoration: "none", "&:hover": { textDecoration: "underline" } }}
              target="_blank"
            >
              {t("dRepProfile.readGuide")}
            </Box>
          </Box>
        </Box>

        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
          {!metadataStatus && (objectives || motivations || qualifications) && (
            <Box component="section" sx={panelSx}>
              <PanelTitle>{t("dRepProfile.about", { name })}</PanelTitle>
              <DRepDetailsInfoItem label={t("forms.dRepData.objectives")} text={objectives} dataTestId="objectives" />
              <DRepDetailsInfoItem label={t("forms.dRepData.motivations")} text={motivations} dataTestId="motivations" />
              <DRepDetailsInfoItem label={t("forms.dRepData.qualifications")} text={qualifications} dataTestId="qualifications" />
            </Box>
          )}
          <DRepVotingTrend
            isError={isDRepVotesError}
            isLoading={areDRepVotesLoading}
            votes={dRepVotes}
          />
          <Box id={VOTING_HISTORY_ID} sx={{ scrollMarginTop: 96 }}>
            <DRepActivityTable
              isError={isDRepVotesError}
              isLoading={areDRepVotesLoading}
              votes={dRepVotes}
            />
          </Box>
        </Box>
      </Box>
    </Box>
  );
};

const panelSx = {
  backgroundColor: "#FFFFFF",
  border: `1px solid ${ds.gray["200"]}`,
  borderRadius: `${RADIUS.lg}px`,
  boxShadow: SHADOWS.card,
  display: "flex",
  flexDirection: "column",
  gap: 2,
  minWidth: 0,
  p: { xxs: 2, md: 2.75 },
};

const PanelTitle = ({ children }: PropsWithChildren) => (
  <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, lineHeight: "24px", m: 0 }}>
    {children}
  </Box>
);

const ExternalLink = ({ href, dataTestId }: { href: string; dataTestId: string }) => (
  <Link
    data-testid={dataTestId}
    href={href}
    rel="noopener noreferrer"
    sx={{ alignItems: "center", color: ds.primary["600"], display: "flex", fontSize: 14, gap: 0.5, minWidth: 0, textDecoration: "none" }}
    target="_blank"
  >
    <Box component="span" sx={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
      {href}
    </Box>
    <ExternalLinkIcon aria-hidden size={14} style={{ flexShrink: 0 }} />
  </Link>
);

const ellipsisStyles = {
  overflow: "hidden",
  textOverflow: "ellipsis",
  whiteSpace: "nowrap",
};

type DrepDetailsInfoItemProps = PropsWithChildren & {
  label: string;
  text?: string | null;
  dataTestId: string;
  isValidating?: boolean;
};

const DRepDetailsInfoItem = ({
  children,
  label,
  text,
  dataTestId,
  isValidating,
}: DrepDetailsInfoItemProps) => {
  if (!children && !text) return null;
  const dataTestIdInfoItemCategoryPrefix = "info-item";
  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 0.5, minWidth: 0 }}>
      {isValidating ? (
        <Skeleton width="128px" height="18px" variant="text" />
      ) : (
        <Typography
          component="h3"
          data-testid={`${dataTestId}-${dataTestIdInfoItemCategoryPrefix}-title`}
          sx={{ color: ds.gray["600"], fontSize: 13, fontWeight: text ? 600 : 400, lineHeight: "18px", m: 0 }}
        >
          {label}
        </Typography>
      )}
      <div
        data-testid={`${dataTestId}-${dataTestIdInfoItemCategoryPrefix}-description`}
      >
        {isValidating ? (
          <Skeleton variant="text" width="100%" height="20px" />
        ) : (
          <>
            {text && (
              <Typography
                sx={{ color: ds.gray["700"], fontSize: 14, lineHeight: "22px", whiteSpace: "pre-line", wordBreak: "break-word" }}
              >
                {text}
              </Typography>
            )}
            {children}
          </>
        )}
      </div>
    </Box>
  );
};

type CopyableTextProps = {
  value: string;
  dataTestId: string;
  isSemiTransparent?: boolean;
  /** Shortened in the middle, for the header line. */
  isCompact?: boolean;
};

const CopyableText = ({
  value,
  dataTestId,
  isSemiTransparent,
  isCompact,
}: CopyableTextProps) => {
  const { t } = useTranslation();
  const { addSuccessAlert } = useSnackbar();
  return (
    <ButtonBase
      aria-label={`${t("clickToCopyLink")}: ${value}`}
      onClick={(e) => {
        navigator.clipboard.writeText(value.toString());
        addSuccessAlert(t("alerts.copiedToClipboard"));
        e.stopPropagation();
      }}
      data-testid={dataTestId}
      sx={{
        borderRadius: `${RADIUS.xs}px`,
        color: isCompact ? ds.gray["700"] : ds.primary["600"],
        gap: 0.75,
        justifyContent: "flex-start",
        maxWidth: "100%",
        "&:hover": { color: ds.primary["700"] },
        "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` },
      }}
    >
      <Box
        component="span"
        sx={{
          ...ellipsisStyles,
          fontFamily: "inherit",
          fontSize: isCompact ? 13 : 14,
          fontWeight: 500,
          opacity: isSemiTransparent ? 0.8 : 1,
        }}
      >
        {isCompact ? `${value.slice(0, 12)}…${value.slice(-6)}` : value}
      </Box>
      <Copy aria-hidden size={14} style={{ flexShrink: 0 }} />
    </ButtonBase>
  );
};

type ReferenceItem = {
  label: string;
  uri: string;
};

const ReferencesGroup = ({ references }: { references: ReferenceItem[] }) => (
  <Box display="flex" flexDirection="column" gap={2}>
    {references.map(({ label, uri }) => (
      <ReferencesLink key={uri} label={label} uri={uri} />
    ))}
  </Box>
);

const ReferencesLink = ({ label, uri }: ReferenceItem) => (
  <Box sx={{ display: "flex", flexDirection: "column", gap: 0.25, minWidth: 0 }}>
    <Typography sx={{ color: ds.gray["600"], fontSize: 13 }}>{label}</Typography>
    <ExternalLink dataTestId={`${testIdFromLabel(label)}-link`} href={uri} />
  </Box>
);
