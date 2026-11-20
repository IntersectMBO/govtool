import { ReactNode, useState } from "react";
import { Box, ButtonBase, Dialog } from "@mui/material";
import { useTranslation } from "react-i18next";
import { Check, Copy, Mail, Share2, X } from "lucide-react";

import { ds, RADIUS } from "@/consts/designTokens";
import { useSnackbar } from "@/context/snackbar";

// Brand glyphs: the pinned lucide-react has no brand icons.
const Glyph = ({ d }: { d: string }) => (
  <Box aria-hidden component="svg" sx={{ flexShrink: 0, height: 16, width: 16 }} viewBox="0 0 24 24">
    <path d={d} fill="currentColor" />
  </Box>
);

const X_PATH =
  "M18.9 2H22l-7.6 8.7L23 22h-6.8l-5.3-6.9L4.8 22H1.7l8.1-9.3L1 2h6.9l4.8 6.3L18.9 2zm-1.2 18h1.9L6.4 3.9H4.4L17.7 20z";
const LINKEDIN_PATH =
  "M20.4 2H3.6A1.6 1.6 0 0 0 2 3.6v16.8A1.6 1.6 0 0 0 3.6 22h16.8a1.6 1.6 0 0 0 1.6-1.6V3.6A1.6 1.6 0 0 0 20.4 2zM8 19H5V9.5h3V19zM6.5 8.2a1.7 1.7 0 1 1 0-3.5 1.7 1.7 0 0 1 0 3.5zM19 19h-3v-4.6c0-1.1 0-2.5-1.5-2.5S12.7 13 12.7 14.3V19h-3V9.5h2.8v1.3h.1a3.1 3.1 0 0 1 2.8-1.5c3 0 3.6 2 3.6 4.6V19z";
const FACEBOOK_PATH =
  "M22 12a10 10 0 1 0-11.6 9.9v-7H7.9V12h2.5V9.8c0-2.5 1.5-3.9 3.8-3.9 1.1 0 2.2.2 2.2.2v2.5h-1.3c-1.2 0-1.6.8-1.6 1.6V12h2.8l-.4 2.9h-2.3v7A10 10 0 0 0 22 12z";

const ShareLink = ({
  href,
  icon,
  label,
  testId,
  color,
}: {
  href: string;
  icon: ReactNode;
  label: string;
  testId: string;
  color?: string;
}) => (
  <Box
    component="a"
    data-testid={testId}
    href={href}
    rel="noopener noreferrer"
    sx={{
      alignItems: "center",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.md}px`,
      color: ds.gray["800"],
      display: "flex",
      flex: 1,
      fontSize: 14,
      fontWeight: 500,
      gap: 1,
      height: 48,
      justifyContent: "center",
      minWidth: 110,
      textDecoration: "none",
      "& svg": { color: color ?? ds.gray["900"] },
      "&:hover": { backgroundColor: ds.gray["50"] },
      "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
    }}
    target="_blank"
  >
    {icon}
    {label}
  </Box>
);

const middle = (value: string) =>
  (value.length > 32 ? `${value.slice(0, 18)}…${value.slice(-12)}` : value);

/** What is shared: a DRep's profile, a governance action, or a forum proposal. */
export type ShareKind = "drep" | "action" | "proposal";

// Each kind's own wording; the buttons' words are shared.
const KIND_KEYS: Record<ShareKind, string> = {
  drep: "drepDashboard.share",
  action: "shareDialog.action",
  proposal: "shareDialog.proposal",
};

type ShareDialogProps = {
  link: string;
  kind?: ShareKind;
  /** Shown in the field, shortened; Copy link still copies the whole URL. */
  displayId?: string;
  buttonTestId?: string;
  copyTestId?: string;
};

/** A round share button and its dialog: copy the link, or share it on. */
export const ShareDialog = ({
  link,
  kind = "drep",
  displayId,
  buttonTestId = "share-drep-button",
  copyTestId = "share-drep-copy",
}: ShareDialogProps) => {
  const { t: translate } = useTranslation();
  const own = new Set(["open", "title", "body", "urlLabel", "hint", "text"]);
  // The kind's wording for its own keys; the DRep dialog's for the buttons.
  const t = (key: string, values?: Record<string, unknown>) => {
    const [, , leaf] = key.split(".");
    return translate(
      key.startsWith("drepDashboard.share.") && own.has(leaf)
        ? `${KIND_KEYS[kind]}.${leaf}`
        : key,
      values,
    );
  };
  const dRepId = displayId;
  const { addSuccessAlert } = useSnackbar();
  const [open, setOpen] = useState(false);
  const [copied, setCopied] = useState(false);
  const url = encodeURIComponent(link);
  const text = encodeURIComponent(t("drepDashboard.share.text"));

  const copy = async () => {
    try {
      await navigator.clipboard.writeText(link);
      setCopied(true);
      addSuccessAlert(t("alerts.copiedToClipboard"));
    } catch {
      setCopied(false);
    }
  };

  return (
    <>
      <ButtonBase
        aria-label={t("drepDashboard.share.open")}
        data-testid={buttonTestId}
        onClick={() => { setCopied(false); setOpen(true); }}
        sx={{
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: "50%",
          color: ds.gray["700"],
          height: 40,
          width: 40,
          "&:hover": { backgroundColor: ds.gray["50"] },
          "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` },
        }}
      >
        <Share2 aria-hidden size={18} />
      </ButtonBase>
      <Dialog
        aria-labelledby="share-drep-title"
        onClose={() => setOpen(false)}
        open={open}
        PaperProps={{ sx: { borderRadius: `${RADIUS.xl}px`, maxWidth: 640, p: { xxs: 2.5, md: 4 }, width: "100%" } }}
      >
        <Box data-testid="share-drep-dialog" sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
          <Box sx={{ alignItems: "flex-start", display: "flex", gap: 2, justifyContent: "space-between" }}>
            <Box>
              <Box component="h2" id="share-drep-title" sx={{ color: ds.gray["900"], fontSize: 28, fontWeight: 600, letterSpacing: "-0.5px", m: 0 }}>
                {t("drepDashboard.share.title")}
              </Box>
              <Box component="p" sx={{ color: ds.gray["600"], fontSize: 16, m: 0, mt: 1 }}>
                {t("drepDashboard.share.body")}
              </Box>
            </Box>
            <ButtonBase
              aria-label={t("drepDashboard.share.close")}
              onClick={() => setOpen(false)}
              sx={{ backgroundColor: ds.gray["100"], borderRadius: "50%", color: ds.gray["700"], flexShrink: 0, height: 36, width: 36, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
            >
              <X aria-hidden size={18} />
            </ButtonBase>
          </Box>
          <Box>
            <Box component="label" htmlFor="share-drep-url" sx={{ color: ds.gray["900"], display: "block", fontSize: 14, fontWeight: 600, mb: 1 }}>
              {t("drepDashboard.share.urlLabel")}
            </Box>
            <Box sx={{ display: "flex", gap: 1.5 }}>
              <Box
                aria-describedby="share-drep-hint"
                component="input"
                data-testid="share-drep-url"
                id="share-drep-url"
                onFocus={(event) => (event.target as HTMLInputElement).select()}
                readOnly
                sx={{
                  backgroundColor: ds.gray["50"],
                  border: `1px solid ${ds.gray["200"]}`,
                  borderRadius: `${RADIUS.md}px`,
                  color: ds.gray["900"],
                  flex: 1,
                  fontFamily: "monospace",
                  fontSize: 13,
                  height: 44,
                  minWidth: 0,
                  px: 1.5,
                  textOverflow: "ellipsis",
                }}
                value={dRepId ? middle(dRepId) : link}
              />
              <ButtonBase
                data-testid={copyTestId}
                onClick={copy}
                sx={{
                  backgroundColor: ds.primary["600"],
                  borderRadius: `${RADIUS.md}px`,
                  color: "#FFFFFF",
                  flexShrink: 0,
                  fontSize: 15,
                  fontWeight: 600,
                  gap: 1,
                  height: 44,
                  px: 2,
                  "&:hover": { backgroundColor: ds.primary["700"] },
                  "&:focus-visible": { outline: `2px solid ${ds.primary["800"]}`, outlineOffset: 2 },
                }}
              >
                {copied ? <Check aria-hidden size={18} /> : <Copy aria-hidden size={18} />}
                {copied ? t("drepDashboard.share.copied") : t("drepDashboard.share.copy")}
              </ButtonBase>
            </Box>
            <Box id="share-drep-hint" sx={{ color: ds.gray["600"], fontSize: 13, mt: 1 }}>
              {t("drepDashboard.share.hint")}
            </Box>
          </Box>
          <Box>
            <Box sx={{ color: ds.gray["600"], fontSize: 14, mb: 1.5 }}>{t("drepDashboard.share.via")}</Box>
            <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1 }}>
              <ShareLink
                href={`https://twitter.com/intent/tweet?url=${url}&text=${text}`}
                icon={<Glyph d={X_PATH} />}
                label={t("drepDashboard.share.x")}
                testId="share-drep-x"
              />
              <ShareLink
                color="#0A66C2"
                href={`https://www.linkedin.com/sharing/share-offsite/?url=${url}`}
                icon={<Glyph d={LINKEDIN_PATH} />}
                label={t("drepDashboard.share.linkedin")}
                testId="share-drep-linkedin"
              />
              <ShareLink
                color="#1877F2"
                href={`https://www.facebook.com/sharer/sharer.php?u=${url}`}
                icon={<Glyph d={FACEBOOK_PATH} />}
                label={t("drepDashboard.share.facebook")}
                testId="share-drep-facebook"
              />
              <ShareLink
                href={`mailto:?subject=${text}&body=${url}`}
                icon={<Mail aria-hidden size={16} />}
                label={t("drepDashboard.share.email")}
                testId="share-drep-email"
              />
            </Box>
          </Box>
        </Box>
      </Dialog>
    </>
  );
};

/** The DRep profile's share button and dialog. */
export const ShareDRepDialog = ({
  dRepId,
  ...props
}: Omit<ShareDialogProps, "kind" | "displayId"> & { dRepId?: string }) => (
  <ShareDialog {...props} displayId={dRepId} kind="drep" />
);
