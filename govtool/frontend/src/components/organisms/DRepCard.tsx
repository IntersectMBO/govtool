import { ReactNode, useEffect, useState } from "react";
import { Link } from "react-router";
import { Avatar, Box, IconButton, Menu, MenuItem, Skeleton } from "@mui/material";
import {
  CircleCheck,
  CircleUserRound,
  Copy,
  EllipsisVertical,
  History,
} from "lucide-react";

import { Button, StatusPill } from "@atoms";
import { PATHS } from "@consts";
import { useModal, useSnackbar } from "@context";
import { useTranslation } from "@hooks";
import {
  DRepData,
  DRepStatus,
  MetadataStandard,
  MetadataValidationStatus,
} from "@models";
import {
  copyToClipboard,
  correctDRepDirectoryFormat,
  encodeCIP129Identifier,
  formatDisplayDate,
  getBase64ImageDetails,
  getMetadataDataMissingStatusTranslation,
} from "@utils";
import { useValidateMutation } from "@/hooks/mutations";
import { useDRepCardStats } from "@/hooks/queries/useDRepDashboard";
import { ds, FONT_FAMILY, RADIUS, SHADOWS } from "@/consts/designTokens";

/** Up to two initials from a name, or "DR" when there is none. */
export const dRepInitials = (name?: string | null) => {
  const words = (name ?? "").trim().split(/\s+/).filter(Boolean);
  if (!words.length) return "DR";
  return (words.length === 1 ? words[0].slice(0, 2) : `${words[0][0]}${words[1][0]}`).toUpperCase();
};

const AVATAR_TONES = [
  { bg: ds.error["50"], text: ds.error["600"] },
  { bg: ds.success["50"], text: ds.success["600"] },
  { bg: ds.primary["50"], text: ds.primary["600"] },
  { bg: ds.orange["50"], text: ds.orange["600"] },
];

/** A stable colour for a DRep's initials avatar. */
export const avatarTone = (seed: string) =>
  AVATAR_TONES[[...seed].reduce((sum, char) => sum + char.charCodeAt(0), 0) % AVATAR_TONES.length];

const statLabelSx = {
  color: ds.gray["500"],
  fontSize: 11,
  fontWeight: 500,
  letterSpacing: "0.2px",
  lineHeight: "16px",
  textTransform: "uppercase" as const,
};

const statValueSx = {
  color: ds.gray["900"],
  fontSize: 14,
  fontWeight: 500,
  lineHeight: "20px",
  whiteSpace: "nowrap" as const,
};

const menuItemSx = {
  color: ds.gray["900"],
  fontSize: 14,
  gap: 1.25,
  py: 1,
  "& svg": { color: ds.primary["600"], flexShrink: 0 },
};

/** One figure under the card's divider; every one after the first is ruled off. */
const Stat = ({
  label,
  children,
  testId,
  isFirst = false,
}: {
  label: string;
  children: ReactNode;
  testId: string;
  isFirst?: boolean;
}) => (
  <Box
    sx={{
      display: "flex",
      flexDirection: "column",
      gap: 0.25,
      minWidth: 0,
      ...(!isFirst && { borderLeft: `1px solid ${ds.gray["200"]}`, pl: 2 }),
    }}
  >
    <Box component="span" data-testid={`${testId}-label`} sx={statLabelSx}>
      {label}
    </Box>
    <Box component="span" data-testid={testId} sx={statValueSx}>
      {children}
    </Box>
  </Box>
);

/** The DRep profile's voting history table, which the card's menu links to. */
export const VOTING_HISTORY_ID = "voting-history";

type DRepCardProps = {
  dRep: DRepData;
  isConnected: boolean;
  isDelegationLoading?: boolean;
  isInProgress?: boolean;
  isMe?: boolean;
  isMyDrep?: boolean;
  onDelegate?: () => void;
  /** Another delegation is being signed or is pending. */
  isDelegateDisabled?: boolean;
};

export const DRepCard = ({
  dRep: {
    status,
    type,
    view,
    votingPower,
    givenName,
    image,
    drepId,
    isScriptBased,
    url,
    metadataHash,
    objectives,
    motivations,
    qualifications,
    latestRegistrationDate,
    delegatorCount,
  },
  isConnected,
  isDelegationLoading,
  isInProgress,
  isMe,
  isMyDrep,
  onDelegate,
  isDelegateDisabled,
}: DRepCardProps) => {
  const { t } = useTranslation();
  const [menuAnchor, setMenuAnchor] = useState<HTMLElement | null>(null);
  const { addErrorAlert, addSuccessAlert } = useSnackbar();
  const stats = useDRepCardStats({ drepId, view, latestRegistrationDate, delegatorCount });
  const { openModal } = useModal();

  const openChooseWalletModal = () =>
    openModal({
      type: "chooseWallet",
      state: {
        pathToNavigate: PATHS.dRepDirectoryDRep.replace(
          ":dRepId",
          view,
        ),
      },
    });

  const cip129Identifier = encodeCIP129Identifier({
    txID: `${isScriptBased ? "23" : "22"}${drepId}`,
    bech32Prefix: "drep",
  });

  const base64Image = getBase64ImageDetails(image ?? "");
  const [isValidating, setIsValidating] = useState(false);
  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const { validateMetadata } = useValidateMutation();

  useEffect(() => {
    if (!url) return;

    const validate = async () => {
      setIsValidating(true);

      const { status: validationStatus } = await validateMetadata({
        standard: MetadataStandard.CIP119,
        url,
        hash: metadataHash ?? "",
      });

      setMetadataStatus(validationStatus);
      setIsValidating(false);
    };
    validate();
  }, [url]);

  const detailsPath = PATHS.dRepDirectoryDRep.replace(":dRepId", view);
  const name = givenName || `${view.slice(0, 10)}…${view.slice(-6)}`;
  // Text from a document that failed validation is not shown.
  const summary = metadataStatus
    ? undefined
    : objectives || motivations || qualifications;
  const tone = avatarTone(view);
  const imageSrc = image
    ? base64Image.isValidBase64Image
      ? `${base64Image.base64Prefix}${image}`
      : image
    : undefined;
  const canDelegate = ["Active", "Inactive"].includes(status);

  const copy = async (value: string) => {
    setMenuAnchor(null);
    if (await copyToClipboard(value)) addSuccessAlert(t("alerts.copiedToClipboard"));
    else addErrorAlert(t("alerts.copyFailed"));
  };
  // The menu offers delegation wherever the card's own button would.
  const canDelegateHere = canDelegate && !isMyDrep && !isInProgress
    && (isConnected ? !!onDelegate : true);
  const delegateFromMenu = () => {
    setMenuAnchor(null);
    if (isConnected) onDelegate?.();
    else openChooseWalletModal();
  };
  let participation = "—";
  if (stats.participation !== null) participation = `${stats.participation}%`;

  return (
    <Box
      component="article"
      data-testid={`${view}-${
        onDelegate
          ? "drep"
          : isInProgress
            ? "delegation-in-progress"
            : "delegated"
      }-card`}
      sx={{
        backgroundColor: "#FFFFFF",
        border: `1px solid ${
          isInProgress
            ? "#FFCBAD"
            : metadataStatus
              ? "#F6D5D5"
              : isMe || isMyDrep
                ? ds.primary["200"]
                : ds.gray["200"]
        }`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flex: 1,
        flexDirection: "column",
        gap: 2,
        minWidth: 0,
        p: 2.5,
        position: "relative",
      }}
    >
      <Box sx={{ alignItems: "flex-start", display: "flex", gap: 1.5 }}>
        {isValidating ? (
          <Skeleton height={40} variant="circular" width={40} />
        ) : (
          <Avatar
            alt=""
            aria-hidden
            data-testid="drep-image"
            src={imageSrc}
            sx={{
              backgroundColor: tone.bg,
              color: tone.text,
              fontSize: 14,
              fontWeight: 600,
              height: 40,
              width: 40,
            }}
          >
            {dRepInitials(givenName)}
          </Avatar>
        )}
        <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 0.5, minWidth: 0 }}>
          {isValidating ? (
            <Skeleton height={22} variant="text" width={120} />
          ) : (
            <Box
              component={type === "DRep" ? Link : "span"}
              data-testid={type === "DRep" ? `${view}-view-details-button` : undefined}
              state={{ enteredFromWithinApp: true }}
              to={detailsPath}
              sx={{
                color: metadataStatus ? ds.error["600"] : ds.gray["900"],
                fontSize: 15,
                fontWeight: 600,
                lineHeight: "22px",
                overflow: "hidden",
                textDecoration: "none",
                textOverflow: "ellipsis",
                whiteSpace: "nowrap",
                "&:hover": { color: ds.primary["600"] },
              }}
            >
              {metadataStatus ? getMetadataDataMissingStatusTranslation(metadataStatus) : name}
            </Box>
          )}
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 0.75 }}>
            <StatusPill dataTestId={`${view}-${status}-pill`} status={status} />
            {isMe && <StatusPill dataTestId={`${view}-yourself-pill`} status={DRepStatus.Yourself} />}
            {isMyDrep && !isMe && (
              <Box component="span" data-testid={`${view}-my-drep-pill`} sx={{ color: ds.primary["600"], fontSize: 12, fontWeight: 600 }}>
                {t("dRepDirectory.card.yourDRep")}
              </Box>
            )}
            {isInProgress && (
              <Box component="span" sx={{ color: ds.orange["700"], fontSize: 12, fontWeight: 600 }}>
                {t("inProgress")}
              </Box>
            )}
          </Box>
        </Box>
        <IconButton
          aria-expanded={Boolean(menuAnchor)}
          aria-haspopup="menu"
          aria-label={t("dRepDirectory.card.moreActionsFor", { name })}
          data-testid={`${view}-menu-button`}
          onClick={(event) => setMenuAnchor(event.currentTarget)}
          size="small"
          sx={{ border: `1px solid ${ds.gray["200"]}`, color: ds.gray["700"], height: 32, width: 32 }}
        >
          <EllipsisVertical aria-hidden size={16} />
        </IconButton>
        <Menu
          anchorEl={menuAnchor}
          anchorOrigin={{ horizontal: "right", vertical: "bottom" }}
          onClose={() => setMenuAnchor(null)}
          open={Boolean(menuAnchor)}
          transformOrigin={{ horizontal: "right", vertical: "top" }}
        >
          {canDelegateHere && (
            <MenuItem
              data-testid={`${view}-menu-delegate`}
              disabled={isConnected && isDelegateDisabled}
              onClick={delegateFromMenu}
              sx={menuItemSx}
            >
              <CircleCheck aria-hidden size={18} />
              {t("dRepDirectory.card.delegateVotingPower")}
            </MenuItem>
          )}
          {type === "DRep" && (
            <MenuItem
              component={Link}
              data-testid={`${view}-menu-view-profile`}
              onClick={() => setMenuAnchor(null)}
              state={{ enteredFromWithinApp: true }}
              sx={menuItemSx}
              to={detailsPath}
            >
              <CircleUserRound aria-hidden size={18} />
              {t("dRepDirectory.card.viewProfile")}
            </MenuItem>
          )}
          {type === "DRep" && (
            <MenuItem
              component={Link}
              data-testid={`${view}-menu-voting-history`}
              onClick={() => setMenuAnchor(null)}
              state={{ enteredFromWithinApp: true }}
              sx={menuItemSx}
              to={`${detailsPath}#${VOTING_HISTORY_ID}`}
            >
              <History aria-hidden size={18} />
              {t("dRepDirectory.card.viewVotingHistory")}
            </MenuItem>
          )}
          <MenuItem
            data-testid={`${cip129Identifier}-copy-id-button`}
            onClick={() => copy(cip129Identifier)}
            sx={menuItemSx}
          >
            <Copy aria-hidden size={18} />
            {t("dRepDirectory.card.copyId")}
          </MenuItem>
          <MenuItem
            data-testid={`${view}-copy-legacy-id-button`}
            onClick={() => copy(view)}
            sx={menuItemSx}
          >
            <Copy aria-hidden size={18} />
            {t("dRepDirectory.card.copyLegacyId")}
          </MenuItem>
        </Menu>
      </Box>

      <Box
        sx={{
          color: summary ? ds.gray["600"] : ds.gray["400"],
          display: "-webkit-box",
          fontSize: 14,
          lineHeight: "20px",
          minHeight: 60,
          overflow: "hidden",
          WebkitBoxOrient: "vertical",
          WebkitLineClamp: 3,
          wordBreak: "break-word",
        }}
      >
        {isValidating ? <Skeleton variant="text" /> : summary || t("dRepDirectory.card.noSummary")}
      </Box>

      <Box sx={{ borderTop: `1px dashed ${ds.gray["200"]}`, mt: "auto" }} />

      <Box sx={{ alignItems: "flex-end", display: "flex", flexWrap: "wrap", gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 2, rowGap: 1.5 }}>
          <Stat isFirst label={t("votingPower")} testId={`${view}-voting-power`}>
            <Box component="span" sx={{ fontFamily: FONT_FAMILY.sans }}>
              {`₳\u00a0${correctDRepDirectoryFormat(votingPower)}`}
            </Box>
          </Stat>
          <Stat label={t("dRepDirectory.card.participation")} testId={`${view}-participation`}>
            {stats.isParticipationLoading ? (
              <Skeleton variant="text" width={40} />
            ) : (
              <Box component="span" sx={{ color: stats.participation === null ? ds.gray["500"] : ds.primary["600"] }}>
                {participation}
              </Box>
            )}
          </Stat>
          {stats.areDelegatorsServed ? (
            <Stat label={t("dRepDirectory.card.delegators")} testId={`${view}-delegators`}>
              {stats.areDelegatorsLoading ? (
                <Skeleton variant="text" width={32} />
              ) : (
                stats.delegators?.toLocaleString("en-US") ?? "—"
              )}
            </Stat>
          ) : (
            // Until the backend serves delegators, the registration date stands in.
            latestRegistrationDate && (
              <Stat label={t("dRepDirectory.card.registered")} testId={`${view}-registered`}>
                {formatDisplayDate(latestRegistrationDate, "MMM d, yyyy")}
              </Stat>
            )
          )}
        </Box>
        {canDelegate && isConnected && onDelegate && !isMyDrep && !isInProgress && (
          <Button
            data-testid={`${view}-delegate-button`}
            disabled={isDelegateDisabled && !isDelegationLoading}
            isLoading={isDelegationLoading}
            onClick={onDelegate}
            size="small"
          >
            {t("delegate")}
          </Button>
        )}
        {canDelegate && !isConnected && (
          <Button
            data-testid={`${view}-connect-to-delegate-button`}
            onClick={openChooseWalletModal}
            size="small"
            variant="outlined"
          >
            {t("connectToDelegate")}
          </Button>
        )}
      </Box>
    </Box>
  );
};
