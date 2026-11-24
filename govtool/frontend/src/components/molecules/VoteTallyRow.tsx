import { Box } from "@mui/material";

import { useTranslation } from "@hooks";
import type { GovernanceActionVoteAggregate } from "@models";
import { ds, FONT_FAMILY } from "@/consts/designTokens";
import { voteBar } from "@/utils/governanceActionFormat";

type Role = GovernanceActionVoteAggregate["role"];

const ROLE_LABELS: Record<Role, string> = { drep: "DRep", spo: "SPO", cc: "CC" };

const mono = { fontFamily: FONT_FAMILY.mono, fontSize: 12, lineHeight: "16px" };

type Props = {
  voterRole: Role;
  /** None means the role does not vote on this action. */
  aggregate?: GovernanceActionVoteAggregate;
  /** "inline": label, bar and figures on one row; "stacked": bar above them. */
  variant?: "inline" | "stacked";
  /** The tally is still loading or could not be read: no bar, no "not eligible". */
  isUnknown?: boolean;
  /** False for actions without pass or fail (Info actions): no green yes share. */
  showPassing?: boolean;
  dataTestId?: string;
};

/**
 * One voting role's tally: yes in green, explicit no in red, a threshold mark,
 * and "yes% / threshold%". A hatched empty bar when the role is not eligible.
 */
export const VoteTallyRow = ({
  voterRole,
  aggregate,
  variant = "inline",
  isUnknown = false,
  showPassing = true,
  dataTestId,
}: Props) => {
  const { t } = useTranslation();
  const bar = aggregate ? voteBar(aggregate) : undefined;
  const isStacked = variant === "stacked";

  const track = (
    <Box
      sx={{
        backgroundColor: ds.gray["100"],
        backgroundImage: aggregate || isUnknown
          ? "none"
          : `repeating-linear-gradient(135deg, ${ds.gray["200"]} 0 2px, transparent 2px 4px)`,
        borderRadius: "3px",
        gridColumn: isStacked ? "1 / -1" : undefined,
        height: 6,
        position: "relative",
      }}
    >
      {bar && (
        <>
          <Box sx={{ backgroundColor: ds.success["600"], borderRadius: "3px 0 0 3px", height: "100%", left: 0, position: "absolute", width: `${bar.yesPercent}%` }} />
          <Box sx={{ backgroundColor: ds.error["600"], height: "100%", left: `${bar.yesPercent}%`, position: "absolute", width: `${bar.votedNoPercent}%` }} />
          <Box aria-hidden sx={{ backgroundColor: ds.gray["500"], height: 12, left: `${bar.thresholdPercent}%`, position: "absolute", top: -3, width: "2px" }} />
        </>
      )}
    </Box>
  );

  const label = (
    <Box component="span" sx={{ ...mono, color: ds.gray["600"], textTransform: isStacked ? "uppercase" : undefined }}>
      {ROLE_LABELS[voterRole]}
    </Box>
  );

  const figures = (
    <Box component="span" sx={{ ...mono, color: ds.gray["500"], minWidth: isStacked ? undefined : 104, textAlign: "right", whiteSpace: "nowrap" }}>
      {!aggregate && !isUnknown && t("landing.pulse.notEligible").toUpperCase()}
      {((aggregate && !bar) || (!aggregate && isUnknown)) && "—"}
      {bar && (
        <>
          <Box component="span" sx={{ color: showPassing && bar.passing ? ds.success["600"] : ds.gray["900"], fontWeight: 600 }}>
            {`${bar.yesPercent.toFixed(1)}%`}
          </Box>
          <Box component="span" sx={{ color: ds.gray["900"] }}>
            {` / ${bar.thresholdPercent.toFixed(1)}%`}
          </Box>
        </>
      )}
    </Box>
  );

  return (
    <Box
      data-testid={dataTestId}
      sx={{
        alignItems: "center",
        columnGap: 1.5,
        display: "grid",
        gridTemplateColumns: isStacked ? "1fr auto" : "36px 1fr auto",
        rowGap: 1,
      }}
    >
      {isStacked ? (
        <>
          {track}
          {label}
          {figures}
        </>
      ) : (
        <>
          {label}
          {track}
          {figures}
        </>
      )}
    </Box>
  );
};
