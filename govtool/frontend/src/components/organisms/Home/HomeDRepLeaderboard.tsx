import { Box } from "@mui/material";
import { useNavigate } from "react-router";

import { Button } from "@atoms";
import { PATHS } from "@consts";
import { useGetDRepListPaginatedQuery, useTranslation } from "@hooks";
import { DRepListSort, DRepStatus } from "@models";
import { correctAdaFormatWithSuffix } from "@utils";
import { ds, RADIUS } from "@/consts/designTokens";

import { DisplayHeading, Eyebrow, HomeColumn, Lead } from "./homeParts";

// Avatar tints in the order the design uses them.
const TINTS = [
  { bg: ds.error["50"], text: ds.error["600"] },
  { bg: ds.orange["50"], text: ds.orange["600"] },
  { bg: ds.primary["50"], text: ds.primary["600"] },
];

const initials = (name: string) =>
  name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((part) => part[0]?.toUpperCase())
    .join("");

/** The three active DReps holding the most voting power. */
export const HomeDRepLeaderboard = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  // A few extra, so the three shown can skip DReps that asked not to be listed.
  const { dRepData } = useGetDRepListPaginatedQuery({
    page: 0,
    pageSize: 6,
    sorting: DRepListSort.VotingPower,
    status: [DRepStatus.Active],
  });
  const dReps = (dRepData ?? []).filter((dRep) => !dRep.doNotList).slice(0, 3);

  return (
    <Box
      component="section"
      sx={{
        background:
          "radial-gradient(60% 80% at 100% 100%, rgba(255, 240, 226, 0.53) 0%, rgba(255, 240, 226, 0) 100%), radial-gradient(50% 80% at 0% 0%, rgba(231, 239, 252, 0.53) 0%, rgba(231, 239, 252, 0) 100%), #F7F8F6",
        borderBottom: "1px solid #E7EAE5",
        borderTop: "1px solid #E7EAE5",
        py: { xxs: 6, md: 7.5 },
      }}
    >
      <HomeColumn>
        <Box sx={{ alignItems: "center", display: "flex", flexDirection: "column", textAlign: "center" }}>
          <Eyebrow color="#74807C">{t("landing.leaderboard.eyebrow")}</Eyebrow>
          <DisplayHeading sx={{ maxWidth: 520, mt: 2 }}>{t("landing.leaderboard.title")}</DisplayHeading>
          <Lead sx={{ maxWidth: 340, mt: 2 }}>{t("landing.leaderboard.description")}</Lead>
        </Box>
        <Box sx={{ display: "grid", gap: 2, gridTemplateColumns: { xxs: "1fr", md: "repeat(3, 1fr)" }, mt: 4 }}>
          {dReps.map((dRep, index) => {
            const name = dRep.givenName || t("landing.leaderboard.anonymous");
            const tint = TINTS[index % TINTS.length];
            const bio = dRep.objectives || dRep.motivations || dRep.qualifications;
            return (
              <Box
                key={dRep.view}
                data-testid="leaderboard-drep-card"
                sx={{
                  backgroundColor: "#FFFFFF",
                  border: `1px solid ${ds.gray["200"]}`,
                  borderRadius: `${RADIUS.lg}px`,
                  boxShadow: "0px 1px 2px 0px rgba(16, 24, 40, 0.04)",
                  display: "flex",
                  flexDirection: "column",
                  gap: 2,
                  p: 2,
                }}
              >
                <Box sx={{ alignItems: "center", display: "flex", gap: 1.5 }}>
                  <Box aria-hidden sx={{ alignItems: "center", backgroundColor: tint.bg, borderRadius: "50%", color: tint.text, display: "flex", flexShrink: 0, fontSize: 18, fontWeight: 700, height: 44, justifyContent: "center", width: 44 }}>
                    {initials(name)}
                  </Box>
                  <Box sx={{ minWidth: 0 }}>
                    <Box component="p" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, letterSpacing: "-0.18px", lineHeight: "24px", m: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                      {name}
                    </Box>
                    <Box component="span" sx={{ alignItems: "center", backgroundColor: ds.success["50"], borderRadius: `${RADIUS.pill}px`, color: ds.success["700"], display: "inline-flex", fontSize: 10, fontWeight: 700, gap: 0.5, lineHeight: "13px", px: 0.75, py: 0.25, textTransform: "uppercase" }}>
                      <Box component="span" sx={{ backgroundColor: ds.success["600"], borderRadius: "50%", height: 5, width: 5 }} />
                      {dRep.status}
                    </Box>
                  </Box>
                </Box>
                <Box>
                  <Box component="span" sx={{ color: ds.gray["500"], display: "block", fontSize: 10, fontWeight: 600, lineHeight: "13px", textTransform: "uppercase" }}>
                    {t("landing.leaderboard.votingPower")}
                  </Box>
                  <Box component="span" sx={{ color: ds.primary["600"], fontSize: 14, fontWeight: 600, lineHeight: "20px" }}>
                    {`₳ ${correctAdaFormatWithSuffix(dRep.votingPower, 1)}`}
                  </Box>
                </Box>
                <Lead sx={{ display: "-webkit-box", flex: 1, overflow: "hidden", WebkitBoxOrient: "vertical", WebkitLineClamp: 3, whiteSpace: "normal" }}>
                  {bio}
                </Lead>
                <Button
                  onClick={() => navigate(PATHS.dRepDirectoryDRep.replace(":dRepId", dRep.view))}
                  sx={{ color: ds.gray["900"], width: "100%" }}
                  variant="outlined"
                >
                  {t("landing.leaderboard.viewProfile")}
                </Button>
              </Box>
            );
          })}
        </Box>
      </HomeColumn>
    </Box>
  );
};
