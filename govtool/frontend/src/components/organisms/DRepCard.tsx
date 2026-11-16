import { useState, useEffect } from "react";
import { Link } from "react-router";
import { Avatar, Box, IconButton, Menu, MenuItem, Skeleton } from "@mui/material";
import { EllipsisVertical } from "lucide-react";

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
  correctDRepDirectoryFormat,
  encodeCIP129Identifier,
  formatDisplayDate,
  getBase64ImageDetails,
  getMetadataDataMissingStatusTranslation,
} from "@utils";
import { useValidateMutation } from "@/hooks/mutations";
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
  const { addSuccessAlert } = useSnackbar();
  const { openModal } = useModal();

  const openChooseWalletModal = () =>
    openModal({
      type: "chooseWallet",
      state: {
        pathToNavigate: PATHS.dashboardDRepDirectoryDRep.replace(
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

  const detailsPath = (
    isConnected ? PATHS.dashboardDRepDirectoryDRep : PATHS.dRepDirectoryDRep
  ).replace(":dRepId", view);
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

  const copyId = () => {
    navigator.clipboard.writeText(cip129Identifier);
    addSuccessAlert(t("alerts.copiedToClipboard"));
    setMenuAnchor(null);
  };

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
              <Box component="span" sx={{ color: ds.orange["600"], fontSize: 12, fontWeight: 600 }}>
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
          {type === "DRep" && (
            <MenuItem
              component={Link}
              onClick={() => setMenuAnchor(null)}
              state={{ enteredFromWithinApp: true }}
              to={detailsPath}
            >
              {t("viewDetails")}
            </MenuItem>
          )}
          <MenuItem data-testid={`${cip129Identifier}-copy-id-button`} onClick={copyId}>
            {t("dRepDirectory.card.copyId")}
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
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 2.5 }}>
          <Box sx={{ display: "flex", flexDirection: "column", gap: 0.25 }}>
            <Box component="span" data-testid={`${view}-voting-power-label`} sx={statLabelSx}>
              {t("votingPower")}
            </Box>
            <Box component="span" data-testid={`${view}-voting-power`} sx={{ ...statValueSx, fontFamily: FONT_FAMILY.sans }}>
              {`₳\u00a0${correctDRepDirectoryFormat(votingPower)}`}
            </Box>
          </Box>
          {latestRegistrationDate && (
            <Box sx={{ borderLeft: `1px solid ${ds.gray["200"]}`, display: "flex", flexDirection: "column", gap: 0.25, pl: 2.5 }}>
              <Box component="span" sx={statLabelSx}>
                {t("dRepDirectory.card.registered")}
              </Box>
              <Box component="span" sx={statValueSx}>
                {formatDisplayDate(latestRegistrationDate, "MMM d, yyyy")}
              </Box>
            </Box>
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
