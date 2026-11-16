import { useState } from "react";
import { Box, Skeleton } from "@mui/material";
import { Link } from "react-router";
import { formatDistanceToNow } from "date-fns";

import { GOV_ACTION_HISTORY_PATHS } from "@consts";
import { useTranslation } from "@hooks";
import type { VotedProposal } from "@models";
import { getFullGovActionId, getProposalTypeLabel } from "@utils";
import { PaginationFooter } from "@/components/molecules/PaginationFooter";
import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";

const PAGE_SIZE = 10;

const VOTE_TONES = {
  yes: PILL_TONES.success,
  no: PILL_TONES.error,
  abstain: PILL_TONES.neutral,
};

type Props = {
  votes?: VotedProposal[];
  isLoading?: boolean;
  isError?: boolean;
};

/** The governance actions a DRep voted on, newest first, ten per page. */
export const DRepActivityTable = ({ votes = [], isLoading, isError }: Props) => {
  const { t } = useTranslation();
  const [page, setPage] = useState(1);
  const sorted = [...votes].sort(
    (a, b) =>
      new Date(b.vote?.date || 0).getTime() - new Date(a.vote?.date || 0).getTime(),
  );
  const rows = sorted.slice((page - 1) * PAGE_SIZE, page * PAGE_SIZE);

  return (
    <Box
      component="section"
      data-testid="drep-activity"
      sx={{
        backgroundColor: "#FFFFFF",
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: "column",
        gap: 2,
        p: { xxs: 2, md: 2.75 },
      }}
    >
      <Box>
        <Box component="h2" id="drep-activity-title" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, lineHeight: "24px", m: 0 }}>
          {t("dRepProfile.activity.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["500"], fontSize: 13, m: 0 }}>
          {t("dRepProfile.activity.subtitle")}
        </Box>
      </Box>

      {isLoading && <Skeleton height={160} variant="rounded" />}
      {!isLoading && (isError || !votes.length) && (
        <Box sx={{ color: ds.gray["500"], fontSize: 14, py: 3, textAlign: "center" }}>
          {t(isError ? "dRepProfile.activity.error" : "dRepProfile.activity.empty")}
        </Box>
      )}
      {!isLoading && !isError && votes.length > 0 && (
        <>
          <Box sx={{ overflowX: "auto" }}>
            <Box aria-labelledby="drep-activity-title" component="table" sx={{ borderCollapse: "collapse", minWidth: 520, width: "100%" }}>
              <Box component="thead">
                <Box component="tr" sx={{ backgroundColor: ds.gray["50"] }}>
                  {[t("dRepProfile.activity.action"), t("dRepProfile.activity.type"), t("dRepProfile.activity.vote")].map(
                    (heading, position) => (
                      <Box
                        key={heading}
                        component="th"
                        scope="col"
                        sx={{ color: ds.gray["600"], fontSize: 12, fontWeight: 500, px: 1.5, py: 1, textAlign: position === 2 ? "right" : "left" }}
                      >
                        {heading}
                      </Box>
                    ),
                  )}
                </Box>
              </Box>
              <Box component="tbody">
                {rows.map(({ proposal, vote }) => {
                  const actionId = getFullGovActionId(proposal.txHash, proposal.index);
                  const tone = vote ? VOTE_TONES[vote.vote] : PILL_TONES.neutral;
                  const when = vote?.date
                    ? formatDistanceToNow(new Date(vote.date), { addSuffix: true })
                    : undefined;
                  return (
                    <Box key={actionId} component="tr" data-testid={`${actionId}-activity-row`} sx={{ borderTop: `1px solid ${ds.gray["100"]}` }}>
                      <Box component="td" sx={{ maxWidth: 360, px: 1.5, py: 1.25 }}>
                        <Box
                          component={Link}
                          sx={{ color: ds.gray["900"], display: "block", fontSize: 14, fontWeight: 500, overflow: "hidden", textDecoration: "none", textOverflow: "ellipsis", whiteSpace: "nowrap", "&:hover": { color: ds.primary["600"], textDecoration: "underline" } }}
                          to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${actionId}`}
                        >
                          {proposal.title || t("govActions.card.untitled")}
                        </Box>
                        <Box sx={{ color: ds.gray["500"], fontSize: 12 }}>
                          {[when, vote?.epochNo ? t("dRepProfile.activity.votedOnEpoch", { epoch: vote.epochNo }) : undefined]
                            .filter(Boolean)
                            .join(" · ")}
                        </Box>
                      </Box>
                      <Box component="td" sx={{ color: ds.gray["700"], fontSize: 13, px: 1.5, py: 1.25, whiteSpace: "nowrap" }}>
                        {getProposalTypeLabel(proposal.type)}
                      </Box>
                      <Box component="td" sx={{ px: 1.5, py: 1.25, textAlign: "right" }}>
                        {vote && (
                          <Box
                            component="span"
                            sx={{ backgroundColor: tone.bg, border: `1px solid ${tone.border}`, borderRadius: `${RADIUS.pill}px`, color: tone.text, fontSize: 12, fontWeight: 500, px: 1, py: 0.25, whiteSpace: "nowrap" }}
                          >
                            {t(`votes.${vote.vote}`)}
                          </Box>
                        )}
                      </Box>
                    </Box>
                  );
                })}
              </Box>
            </Box>
          </Box>
          {votes.length > PAGE_SIZE && (
            <PaginationFooter
              onPageChange={setPage}
              onPageSizeChange={() => {}}
              page={page}
              pageSize={PAGE_SIZE}
              total={votes.length}
            />
          )}
        </>
      )}
    </Box>
  );
};
