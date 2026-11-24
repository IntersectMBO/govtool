import { ReactNode, useEffect, useId, useRef, useState } from "react";
import { Link as RouterLink, useLocation } from "react-router";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase, Popover } from "@mui/material";
import {
  ChevronRight,
  FileText,
  Landmark,
  LayoutGrid,
  LogOut,
  ShieldCheck,
  UserRound,
  Vote,
} from "lucide-react";

import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";
import { PATHS, PDF_PATHS } from "@/consts/paths";
import { useFeatureFlag } from "@/context/featureFlag";
import { useProposalDiscussion } from "@/context/proposalDiscussion";
import { useCardano } from "@/context/wallet";
import { useDRepDashboardVotes } from "@/hooks/queries/useDRepDashboard";
import { useGetDRepDetailsQuery } from "@/hooks/queries/useGetDrepDetailsQuery";
import { useGetLiveProposalsQuery } from "@/hooks/queries/useGetLiveProposalsQuery";
import { useGetVoterInfo } from "@/hooks/queries/useGetVoterInfoQuery";
import { useDisconnectWallet } from "@/hooks/useConnectedWallet";
import { isRetiredDRep, stakeHexToBech32 } from "@/utils/drepDashboard";
import { pendingVoteCount, subscribeProfileMenu } from "@/utils/profileMenu";

const focusRing = {
  "&.Mui-focusVisible, &:focus-visible": {
    outline: `2px solid ${ds.primary["600"]}`,
    outlineOffset: 2,
  },
};

const initials = (name: string) =>
  name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((part) => part[0]?.toUpperCase() ?? "")
    .join("");

const Avatar = ({ name, size }: { name?: string; size: number }) => {
  const letters = name ? initials(name) : "";
  return (
    <Box
      aria-hidden
      sx={{
        alignItems: "center",
        background: `linear-gradient(135deg, ${ds.orange["700"]} 0%, ${ds.orange["750"]} 100%)`,
        borderRadius: "50%",
        color: "#FFFFFF",
        display: "flex",
        flexShrink: 0,
        fontSize: size > 40 ? 17 : 15,
        fontWeight: 600,
        height: size,
        justifyContent: "center",
        width: size,
      }}
    >
      {letters || <UserRound size={size / 2.2} />}
    </Box>
  );
};

type Role = { id: string; label: string; status?: string; tone: keyof typeof PILL_TONES };

const RoleChip = ({ role }: { role: Role }) => (
  <Box
    data-testid={`profile-role-${role.id}`}
    sx={{
      alignItems: "center",
      backgroundColor: ds.gray["50"],
      borderRadius: `${RADIUS.md}px`,
      color: ds.gray["900"],
      display: "flex",
      fontSize: 14,
      fontWeight: 500,
      gap: 0.75,
      minWidth: 0,
      px: 1.25,
      py: 1,
    }}
  >
    <ShieldCheck aria-hidden color={ds.primary["600"]} size={16} style={{ flexShrink: 0 }} />
    <Box component="span" sx={{ flex: 1 }}>{role.label}</Box>
    {role.status && (
    <Box
      component="span"
      sx={{
        backgroundColor: PILL_TONES[role.tone].bg,
        borderRadius: `${RADIUS.pill}px`,
        color: PILL_TONES[role.tone].text,
        fontSize: 11,
        fontWeight: 600,
        px: 0.75,
        py: 0.125,
      }}
    >
      {role.status}
    </Box>
    )}
  </Box>
);

const MenuRow = ({
  icon,
  label,
  body,
  to,
  isCurrent,
  trailing,
  testId,
  onSelect,
}: {
  icon: ReactNode;
  label: string;
  body?: string;
  to: string;
  isCurrent: boolean;
  trailing?: ReactNode;
  testId: string;
  onSelect: () => void;
}) => (
  <Box
    aria-current={isCurrent ? "page" : undefined}
    component={RouterLink}
    data-testid={testId}
    onClick={onSelect}
    sx={{
      alignItems: "center",
      backgroundColor: isCurrent ? ds.primary["25"] : "transparent",
      border: `1px solid ${isCurrent ? ds.primary["200"] : "transparent"}`,
      borderRadius: `${RADIUS.md}px`,
      color: ds.gray["900"],
      display: "flex",
      gap: 1.25,
      px: 1,
      py: 0.75,
      textDecoration: "none",
      "&:hover": { backgroundColor: isCurrent ? ds.primary["25"] : ds.gray["50"] },
      ...focusRing,
    }}
    to={to}
  >
    <Box
      aria-hidden
      sx={{
        alignItems: "center",
        backgroundColor: isCurrent ? ds.primary["50"] : ds.gray["100"],
        borderRadius: `${RADIUS.sm}px`,
        color: isCurrent ? ds.primary["600"] : ds.gray["700"],
        display: "flex",
        flexShrink: 0,
        height: 32,
        justifyContent: "center",
        width: 32,
      }}
    >
      {icon}
    </Box>
    <Box sx={{ flex: 1, minWidth: 0 }}>
      <Box sx={{ color: isCurrent ? ds.primary["700"] : ds.gray["900"], fontSize: 14, fontWeight: isCurrent ? 600 : 500 }}>
        {label}
      </Box>
      {body && <Box sx={{ color: ds.gray["600"], fontSize: 12 }}>{body}</Box>}
    </Box>
    {trailing ?? <ChevronRight aria-hidden color={ds.gray["500"]} size={16} />}
  </Box>
);

/**
 * The connected user's avatar in the top nav, opening their profile: name,
 * governance roles with their status, where to go next, and logout.
 */
export const ProfileMenu = () => {
  const { t } = useTranslation();
  const { pathname, search } = useLocation();
  const { stakeKey, dRepID, pendingTransaction } = useCardano();
  const { voter } = useGetVoterInfo({ enabled: !!stakeKey });
  const { username } = useProposalDiscussion();
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { confirmDisconnect } = useDisconnectWallet();
  const [isOpen, setIsOpen] = useState(false);
  const anchor = useRef<HTMLButtonElement>(null);
  const menuId = useId();

  const isDRep = !!voter?.isRegisteredAsDRep;
  const isDirectVoter = !!voter?.isRegisteredAsSoleVoter;
  const isRetired = !!voter && isRetiredDRep(voter, pendingTransaction);
  const isVoter = isDRep || isDirectVoter;

  // Read only while the menu is open: it is the only place showing them.
  const { dRep } = useGetDRepDetailsQuery(dRepID, { enabled: isOpen && isDRep && !!dRepID });
  const { votes } = useDRepDashboardVotes(isOpen && isVoter ? dRepID : undefined);
  const close = () => setIsOpen(false);
  const { liveProposals, liveProposalsTotal, areLiveProposalsLoading } = useGetLiveProposalsQuery();
  const toVote = isVoter && !areLiveProposalsLoading
    ? pendingVoteCount(liveProposals, liveProposalsTotal, votes)
    : undefined;

  // The wallet popup's View Profile and the connection summary open this.
  useEffect(() => subscribeProfileMenu(() => setIsOpen(true)), []);
  // A link inside the menu leaves it open over the new page otherwise.
  useEffect(() => setIsOpen(false), [pathname, search]);

  const dRepName = voter?.givenName || dRep?.givenName || undefined;
  const name = dRepName || username || undefined;
  const address = stakeKey ? stakeHexToBech32(stakeKey) ?? stakeKey : "";
  // The forum handle under a DRep name; the stake address otherwise.
  const handle = dRepName && username
    ? `@${username}`
    : address && `${address.slice(0, 9)}…${address.slice(-4)}`;

  const roles: Role[] = [];
  if (isDirectVoter) {
    roles.push({ id: "direct-voter", label: t("accountMenu.directVoter"), status: t("accountMenu.statuses.active"), tone: "success" });
  }
  if (isDRep) {
    // No badge until the DRep list says whether it is active.
    const isInactive = dRep?.status === "Inactive";
    roles.push({
      id: "drep",
      label: t("accountMenu.drep"),
      status: dRep
        ? t(isInactive ? "accountMenu.statuses.inactive" : "accountMenu.statuses.active")
        : undefined,
      tone: isInactive ? "neutral" : "success",
    });
  } else if (isRetired) {
    roles.push({ id: "drep", label: t("accountMenu.drep"), status: t("accountMenu.statuses.retired"), tone: "warning" });
  }

  const tab = new URLSearchParams(search).get("tab");
  const onDashboard = pathname === PATHS.dashboard;
  const votingPath = isDRep ? `${PATHS.dashboard}?tab=governance` : PATHS.governanceActions;
  const isVotingCurrent = isDRep ? onDashboard && tab === "governance" : pathname === PATHS.governanceActions;

  return (
    <>
      <ButtonBase
        ref={anchor}
        aria-controls={isOpen ? menuId : undefined}
        aria-expanded={isOpen}
        aria-haspopup="dialog"
        aria-label={t("accountMenu.profileLabel")}
        data-testid="profile-avatar"
        onClick={() => setIsOpen(true)}
        sx={{ borderRadius: "50%", ...focusRing }}
      >
        <Avatar name={name} size={40} />
      </ButtonBase>
      <Popover
        anchorEl={anchor.current}
        anchorOrigin={{ horizontal: "right", vertical: "bottom" }}
        onClose={() => setIsOpen(false)}
        open={isOpen && !!anchor.current}
        slotProps={{
          paper: {
            "aria-label": t("accountMenu.profileLabel"),
            id: menuId,
            role: "dialog",
            sx: { border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.xl}px`, boxShadow: SHADOWS.drop, mt: 1, p: 1.5, width: 340 },
          } as object,
        }}
        transformOrigin={{ horizontal: "right", vertical: "top" }}
      >
        <Box data-testid="profile-menu" sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
          <Box sx={{ alignItems: "center", display: "flex", gap: 1.5, px: 0.5, pt: 0.5 }}>
            <Avatar name={name} size={44} />
            <Box sx={{ minWidth: 0 }}>
              <Box data-testid="profile-menu-name" sx={{ color: ds.gray["900"], fontSize: 17, fontWeight: 600, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                {name ?? t("accountMenu.account")}
              </Box>
              {handle && <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{handle}</Box>}
            </Box>
          </Box>
          {roles.length > 0 && (
            <Box sx={{ borderTop: `1px dashed ${ds.gray["200"]}`, pt: 1.5 }}>
              <Box component="h2" sx={{ color: ds.gray["600"], fontSize: 12, fontWeight: 500, m: 0, mb: 1, px: 0.5 }}>{t("accountMenu.roles")}</Box>
              <Box sx={{ display: "grid", gap: 1, gridTemplateColumns: roles.length > 1 ? "repeat(2, minmax(0, 1fr))" : "1fr" }}>
                {roles.map((role) => <RoleChip key={role.id} role={role} />)}
              </Box>
            </Box>
          )}
          <Box component="nav" sx={{ borderTop: `1px dashed ${ds.gray["200"]}`, display: "flex", flexDirection: "column", gap: 0.5, pt: 1.5 }}>
            <MenuRow
              body={t("accountMenu.overviewBody")}
              icon={<LayoutGrid size={16} />}
              isCurrent={onDashboard && (!tab || tab === "overview")}
              label={t("accountMenu.overview")}
              onSelect={close}
              testId="profile-menu-overview"
              to={PATHS.dashboard}
            />
            <MenuRow
              icon={<Vote size={16} />}
              isCurrent={isVotingCurrent}
              label={t("accountMenu.votingActivity")}
              onSelect={close}
              testId="profile-menu-voting"
              to={votingPath}
              trailing={toVote ? (
                <Box
                  aria-label={t("accountMenu.toVote", { count: toVote })}
                  component="span"
                  role="img"
                  sx={{ backgroundColor: ds.orange["50"], borderRadius: `${RADIUS.pill}px`, color: ds.orange["750"], fontSize: 12, fontWeight: 600, minWidth: 22, px: 0.75, py: 0.25, textAlign: "center" }}
                >
                  {toVote}
                </Box>
              ) : undefined}
            />
            {isProposalDiscussionForumEnabled && (
              <MenuRow
                icon={<FileText size={16} />}
                isCurrent={pathname === PDF_PATHS.proposalDiscussion}
                label={t("accountMenu.myProposals")}
                onSelect={close}
                testId="profile-menu-proposals"
                to={PDF_PATHS.proposalDiscussion}
              />
            )}
            {isDRep && (
              <MenuRow
                icon={<Landmark size={16} />}
                isCurrent={onDashboard && tab === "history"}
                label={t("accountMenu.historyLog")}
                onSelect={close}
                testId="profile-menu-history"
                to={`${PATHS.dashboard}?tab=history`}
              />
            )}
          </Box>
          <Box sx={{ borderTop: `1px dashed ${ds.gray["200"]}`, pt: 1 }}>
            <ButtonBase
              data-testid="profile-menu-logout"
              onClick={() => {
                close();
                confirmDisconnect();
              }}
              sx={{ borderRadius: `${RADIUS.md}px`, color: ds.error["700"], fontFamily: "inherit", fontSize: 14, fontWeight: 500, gap: 1.25, justifyContent: "flex-start", px: 1, py: 0.75, width: "100%", "&:hover": { backgroundColor: ds.error["50"] }, ...focusRing }}
            >
              <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.error["50"], borderRadius: `${RADIUS.sm}px`, display: "flex", height: 32, justifyContent: "center", width: 32 }}>
                <LogOut size={16} />
              </Box>
              {t("accountMenu.logout")}
            </ButtonBase>
          </Box>
        </Box>
      </Popover>
    </>
  );
};
