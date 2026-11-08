import { FC } from "react";
import { Box, Skeleton, Tooltip } from "@mui/material";
import { Link, useLocation } from "react-router";
import { formatDistanceToNow } from "date-fns";
import { Clock, ExternalLink, Info } from "lucide-react";

import {
  GovernanceActionCardHeader,
  VoteTallyRow,
} from "@molecules";
import { useGetGovernanceActionRecordQuery, useTranslation } from "@hooks";
import {
  formatDisplayDate,
  getFullGovActionId,
  getProposalTypeLabel,
  getProposalTypeNoEmptySpaces,
} from "@utils";
import {
  GovernanceActionVoteAggregate,
  Vote,
  MetadataIssue,
  MetadataValidationStatus,
  ProposalData,
} from "@models";
import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";
import { timeLeft } from "@/utils/governanceActionFormat";
import { GovernanceActionType } from "@/types/governanceAction";

type ActionTypeProps = Omit<ProposalData, "id"> & {
  onClick?: () => void;
  inProgress?: boolean;
  isValidating?: boolean;
  metadataStatus?: MetadataValidationStatus;
  metadataIssues?: MetadataIssue[];
  /** The connected voter's vote, shown as a pill; the voted-on list sets it. */
  myVote?: Vote;
  /** Explorer link to the transaction that cast `myVote`. */
  myVoteTxUrl?: string;
  /** Overrides for the title link, used by the voted-on list. */
  link?: { to: string; state: unknown; dataTestId: string };
};

const ROLES: GovernanceActionVoteAggregate["role"][] = ["drep", "spo", "cc"];

const pillSx = (tone: { bg: string; border: string; text: string }) => ({
  backgroundColor: tone.bg,
  border: `1px solid ${tone.border}`,
  borderRadius: `${RADIUS.xs}px`,
  color: tone.text,
  fontSize: 12,
  fontWeight: 500,
  lineHeight: "16px",
  px: 0.75,
  py: 0.125,
  whiteSpace: "nowrap" as const,
});

const MetaItem = ({ label, children }: { label: string; children: React.ReactNode }) => (
  <Box sx={{ display: "flex", flexDirection: "column", gap: 0.25, minWidth: 0 }}>
    <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "16px" }}>
      {label}
    </Box>
    <Box component="span" sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 13, fontWeight: 500, gap: 0.5, lineHeight: "18px" }}>
      {children}
    </Box>
  </Box>
);

/** A live governance action as a row: summary on the left, role tallies on the right. */
export const GovernanceActionCard: FC<ActionTypeProps> = ({
  abstract,
  type,
  inProgress = false,
  expiryDate,
  expiryEpochNo,
  onClick,
  createdDate,
  createdEpochNo,
  txHash,
  index,
  title,
  isValidating,
  metadataStatus,
  metadataIssues,
  myVote,
  myVoteTxUrl,
  link,
  ...otherProposalData
}) => {
  const { t } = useTranslation();
  const govActionId = getFullGovActionId(txHash, index);
  const {
    governanceAction: record,
    isGovernanceActionLoading,
    governanceActionError,
  } = useGetGovernanceActionRecordQuery(govActionId, { refetchInterval: 60000 });
  const typeId = getProposalTypeNoEmptySpaces(type);
  const left = timeLeft(expiryDate);

  const { pathname: currentPath } = useLocation();
  const pathname = currentPath.replace(/governance_actions.*/g, "governance_actions");
  const isCategoryView = currentPath.includes("category");

  return (
    <Box
      component="article"
      data-testid={`govaction-${typeId}-card`}
      sx={{
        backgroundColor: "#FFFFFF",
        border: `1px solid ${
          inProgress ? "#FFCBAD" : metadataStatus ? "#F6D5D5" : ds.gray["200"]
        }`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "grid",
        gridTemplateColumns: { xxs: "1fr", md: "1fr 230px" },
        overflow: "hidden",
        position: "relative",
        transition: "border-color 120ms",
        "&:hover": { borderColor: inProgress ? "#FFCBAD" : ds.gray["300"] },
        "&:focus-within": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
      }}
    >
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5, minWidth: 0, p: { xxs: 2, md: 2.5 } }}>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
          <Box component="span" sx={pillSx(inProgress ? PILL_TONES.warning : PILL_TONES.success)}>
            {inProgress ? t("inProgress") : t("govActions.card.active")}
          </Box>
          {myVote && (
            <Box
              component="span"
              data-testid="my-vote"
              // Above the card's stretched link, so the explorer link is clickable.
              sx={{ alignItems: "center", display: "inline-flex", gap: 0.75, position: "relative", zIndex: 1 }}
            >
              <Box
                component="span"
                data-testid={`my-vote-${myVote}`}
                sx={pillSx(myVote === "yes" ? PILL_TONES.success : myVote === "no" ? PILL_TONES.error : PILL_TONES.neutral)}
              >
                {t("govActions.card.youVoted", { vote: t(`votes.${myVote}`) })}
              </Box>
              {myVoteTxUrl && (
                <Box
                  component="a"
                  data-testid="my-vote-transaction-link"
                  href={myVoteTxUrl}
                  rel="noopener noreferrer"
                  target="_blank"
                  sx={{ alignItems: "center", color: ds.primary["600"], display: "inline-flex", fontSize: 12, fontWeight: 500, gap: 0.25, textDecoration: "none", "&:hover": { textDecoration: "underline" } }}
                >
                  {t("seeTransaction")}
                  <ExternalLink aria-hidden size={12} />
                </Box>
              )}
            </Box>
          )}
          <Box aria-hidden component="span" sx={{ backgroundColor: ds.gray["300"], borderRadius: "50%", height: 4, width: 4 }} />
          <Box
            component="span"
            data-testid={`${typeId}-type`}
            sx={{ ...pillSx(PILL_TONES.neutral), fontWeight: 600 }}
          >
            {getProposalTypeLabel(type)}
          </Box>
          {left && (
            <Box
              component="span"
              sx={{ alignItems: "center", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.pill}px`, color: ds.gray["600"], display: "inline-flex", fontSize: 12, gap: 0.5, lineHeight: "16px", ml: { md: "auto" }, px: 1, py: 0.25 }}
            >
              <Clock aria-hidden size={12} />
              {t("govActions.card.left", { time: left })}
            </Box>
          )}
        </Box>

        {isValidating ? (
          <Skeleton height="24px" variant="rounded" width="60%" />
        ) : (
          <Box
            component={Link}
            data-testid={link?.dataTestId ?? `govaction-${govActionId}-view-detail`}
            onClick={onClick}
            state={link ? link.state : {
              proposal: {
                abstract,
                type,
                inProgress,
                expiryDate,
                expiryEpochNo,
                createdDate,
                createdEpochNo,
                txHash,
                index,
                title,
                isValidating,
                metadataStatus,
                ...otherProposalData,
              },
              openedFromCategoryPage: isCategoryView,
            }}
            to={link?.to ?? `${pathname}/${govActionId}`}
            sx={{
              color: "inherit",
              outline: "none",
              textDecoration: "none",
              // The whole card is the link's hit area.
              "&::after": { content: '""', inset: 0, position: "absolute" },
              "&:hover h3, &:hover p": { color: ds.primary["600"] },
            }}
          >
            <GovernanceActionCardHeader
              compact
              isDataMissing={metadataStatus}
              isValidating={isValidating}
              metadataIssues={metadataIssues}
              title={title || t("govActions.card.untitled")}
            />
          </Box>
        )}

        <Box sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />

        <Box sx={{ display: "flex", flexWrap: "wrap", gap: { xxs: 2, sm: 3 } }}>
          <MetaItem label={t("govActions.card.submitted")}>
            {createdDate
              ? formatDistanceToNow(new Date(createdDate), { addSuffix: true })
              : t("govActions.card.epoch", { epoch: createdEpochNo })}
          </MetaItem>
          <Box aria-hidden sx={{ alignSelf: "center", borderLeft: `1px solid ${ds.gray["200"]}`, height: 24 }} />
          <MetaItem label={t("govActions.card.expires")}>
            {expiryEpochNo !== undefined && t("govActions.card.epoch", { epoch: expiryEpochNo })}
            {left && (
              <Box component="span" sx={{ color: ds.gray["500"], fontWeight: 400 }}>
                {`(${t("govActions.card.left", { time: left })})`}
              </Box>
            )}
            {expiryDate && (
              <Tooltip arrow title={formatDisplayDate(expiryDate, "do MMM yyyy, HH:mm")}>
                <Box
                  aria-label={formatDisplayDate(expiryDate, "do MMM yyyy, HH:mm")}
                  component="span"
                  sx={{ display: "inline-flex", position: "relative", zIndex: 1 }}
                  tabIndex={0}
                >
                  <Info aria-hidden color={ds.gray["400"]} size={13} />
                </Box>
              </Tooltip>
            )}
          </MetaItem>
        </Box>
      </Box>

      <Box
        sx={{
          backgroundColor: ds.gray["25"],
          borderLeft: { md: `1px solid ${ds.gray["200"]}` },
          borderTop: { xxs: `1px solid ${ds.gray["200"]}`, md: "none" },
          display: "flex",
          flexDirection: "column",
          gap: 2,
          justifyContent: "center",
          p: { xxs: 2, md: 2.5 },
        }}
      >
        {ROLES.map((role) => (
          <VoteTallyRow
            key={role}
            aggregate={record?.vote_aggregates?.find((aggregate) => aggregate.role === role)}
            dataTestId={`govaction-${govActionId}-${role}-tally`}
            isUnknown={!record && (isGovernanceActionLoading || !!governanceActionError)}
            showPassing={type !== GovernanceActionType.InfoAction}
            voterRole={role}
            variant="stacked"
          />
        ))}
      </Box>
    </Box>
  );
};
