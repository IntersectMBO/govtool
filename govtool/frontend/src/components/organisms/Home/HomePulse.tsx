import { Box } from "@mui/material";
import { useQueries } from "@tanstack/react-query";
import { useNavigate } from "react-router";
import { formatDistanceToNow } from "date-fns";

import { Button } from "@atoms";
import { PATHS, QUERY_KEYS } from "@consts";
import { useGetLiveProposalsQuery, useTranslation } from "@hooks";
import type {
  GovernanceActionRecord,
  GovernanceActionVoteAggregate,
  ProposalData,
} from "@models";
import { getGovernanceActionRecord } from "@services";
import {
  correctAdaFormatWithSuffix,
  getGovActionPath,
  getProposalTypeLabel,
  toGovernanceActionId,
} from "@utils";
import { ds, FONT_FAMILY, RADIUS } from "@/consts/designTokens";

import { VoteTallyRow } from "@/components/molecules/VoteTallyRow";
import { timeLeft, treasuryTotal } from "@/utils/governanceActionFormat";
import { DisplayHeading, Eyebrow, HomeColumn } from "./homeParts";

const ROLES: GovernanceActionVoteAggregate["role"][] = ["drep", "spo", "cc"];

const mono = {
  fontFamily: FONT_FAMILY.mono,
  fontSize: 12,
  lineHeight: "16px",
};

const Dot = ({ color }: { color: string }) => (
  <Box component="span" sx={{ backgroundColor: color, borderRadius: "2px", display: "inline-block", height: 8, width: 8 }} />
);

const voteCount = (proposal: ProposalData) => {
  const p = proposal as ProposalData & Record<string, number | undefined>;
  return [
    "dRepYesVotes",
    "dRepNoVotes",
    "dRepAbstainVotes",
    "poolYesVotes",
    "poolNoVotes",
    "poolAbstainVotes",
    "ccYesVotes",
    "ccNoVotes",
    "ccAbstainVotes",
  ].reduce((sum, key) => sum + (typeof p[key] === "number" ? (p[key] as number) : 0), 0);
};

const PulseRow = ({
  proposal,
  record,
  isRecordUnknown = false,
}: {
  proposal: ProposalData;
  record?: GovernanceActionRecord;
  isRecordUnknown?: boolean;
}) => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const amount = treasuryTotal(proposal.details);
  const left = timeLeft(proposal.expiryDate);
  const meta = [
    amount !== undefined ? `₳ ${correctAdaFormatWithSuffix(amount)}` : undefined,
    formatDistanceToNow(new Date(proposal.createdDate), { addSuffix: true }),
    t("landing.pulse.epoch", { epoch: proposal.createdEpochNo }),
  ].filter(Boolean);

  return (
    <Box
      data-testid="pulse-row"
      sx={{
        borderBottom: `1px solid ${ds.gray["200"]}`,
        display: "grid",
        gap: { xxs: 2, md: 4 },
        gridTemplateColumns: { xxs: "1fr", md: "1fr 370px" },
        px: { md: 2 },
        py: 3,
      }}
    >
      <Box sx={{ minWidth: 0 }}>
        <Box component="span" sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: `${RADIUS.xs}px`, color: ds.primary["600"], display: "inline-flex", fontSize: 12, fontWeight: 500, gap: 0.5, lineHeight: "18px", px: 0.75 }}>
          <Box component="span" sx={{ backgroundColor: ds.primary["600"], borderRadius: "50%", height: 4, width: 4 }} />
          {getProposalTypeLabel(proposal.type)}
        </Box>
        <Box
          component="button"
          onClick={() => navigate(getGovActionPath(proposal.txHash, proposal.index))}
          type="button"
          sx={{ background: "none", border: "none", color: ds.gray["900"], cursor: "pointer", display: "block", fontFamily: "inherit", fontSize: 16, fontWeight: 600, letterSpacing: "-0.18px", lineHeight: "24px", mt: 1, p: 0, textAlign: "left", "&:hover": { color: ds.primary["600"] } }}
        >
          {proposal.title ?? t("landing.pulse.untitled")}
        </Box>
        <Box sx={{ alignItems: "center", color: ds.gray["500"], display: "flex", flexWrap: "wrap", fontSize: 12, fontWeight: 500, gap: 0.75, lineHeight: "18px", mt: 0.5 }}>
          {meta.map((item, index) => (
            <Box key={item} component="span" sx={{ color: index === 0 && amount !== undefined ? ds.gray["900"] : undefined, fontWeight: index === 0 && amount !== undefined ? 600 : 500 }}>
              {index ? "· " : ""}
              {item}
            </Box>
          ))}
          {left && (
            <Box component="span" sx={{ color: ds.error["600"] }}>
              {`· ● ${t("landing.pulse.left", { time: left })}`}
            </Box>
          )}
        </Box>
      </Box>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.25 }}>
        {ROLES.map((role) => (
          <VoteTallyRow
            key={role}
            aggregate={record?.vote_aggregates?.find((aggregate) => aggregate.role === role)}
            dataTestId={`pulse-${role}-row`}
            isUnknown={isRecordUnknown}
            showPassing={proposal.type !== "InfoAction"}
            voterRole={role}
          />
        ))}
        <Box sx={{ ...mono, alignItems: "center", color: ds.gray["600"], display: "flex", flexWrap: "wrap", gap: 1 }}>
          <Box component="span" sx={{ color: ds.gray["900"], fontWeight: 500 }}>
            {t("landing.pulse.votes", { count: voteCount(proposal) })}
          </Box>
          <span>·</span>
          <Dot color={ds.success["600"]} />
          {t("landing.pulse.yes")}
          <Dot color={ds.error["600"]} />
          {t("landing.pulse.no")}
          <Box component="span" sx={{ borderLeft: `1px solid ${ds.gray["300"]}`, pl: 1 }}>
            {t("landing.pulse.threshold")}
          </Box>
        </Box>
      </Box>
    </Box>
  );
};

/** The three newest actions open for voting, with each role's tally. */
export const HomePulse = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { liveProposals, areLiveProposalsLoading } = useGetLiveProposalsQuery();
  const shown = liveProposals.slice(0, 3);
  const records = useQueries({
    queries: shown.map((proposal) => {
      const actionId = toGovernanceActionId(`${proposal.txHash}#${proposal.index}`);
      return {
        queryKey: [QUERY_KEYS.useGetGovernanceActionRecordKey, actionId],
        queryFn: () => getGovernanceActionRecord(actionId),
      };
    }),
  });

  return (
    <Box component="section" sx={{ backgroundColor: "#FCFCFA", py: { xxs: 6, md: 10 } }}>
      <HomeColumn>
        <Box sx={{ alignItems: { md: "flex-end" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
          <Box sx={{ maxWidth: 400 }}>
            <Eyebrow color="#74807C">{t("landing.pulse.eyebrow")}</Eyebrow>
            <DisplayHeading sx={{ color: "#252723", mt: 2 }}>{t("landing.pulse.title")}</DisplayHeading>
          </Box>
          <Button onClick={() => navigate(PATHS.governanceActions)} sx={{ color: ds.gray["900"] }} variant="outlined">
            {t("landing.pulse.seeAll")}
          </Button>
        </Box>
        <Box sx={{ mt: 3 }}>
          {!areLiveProposalsLoading && shown.length === 0 && (
            <Box component="p" sx={{ color: ds.gray["500"], fontSize: 14, py: 4 }}>
              {t("landing.pulse.empty")}
            </Box>
          )}
          {shown.map((proposal, index) => (
            <PulseRow
              key={proposal.id}
              isRecordUnknown={
                !records[index]?.data &&
                (records[index]?.isLoading || records[index]?.isError)
              }
              proposal={proposal}
              record={records[index]?.data}
            />
          ))}
        </Box>
      </HomeColumn>
    </Box>
  );
};
