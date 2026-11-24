import { useEffect, useState } from "react";
import { Box, ButtonBase, CircularProgress, Dialog } from "@mui/material";
import { useQueryClient } from "@tanstack/react-query";
import { Check, CircleCheck, Copy, Info, TriangleAlert, X } from "lucide-react";

import { Button } from "@atoms";
import { QUERY_KEYS } from "@consts";
import { useAppContext, useCardano, useSnackbar } from "@context";
import {
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useGetVoterInfo,
  useTranslation,
} from "@hooks";
import type { CurrentDelegation, DRepData } from "@models";
import {
  correctAdaFormat,
  correctDRepDirectoryFormat,
  encodeCIP129Identifier,
  getItemFromLocalStorage,
  isSameDRep,
  WALLET_LS_KEY,
} from "@utils";
import { ds, PILL_TONES, RADIUS } from "@/consts/designTokens";

export type DelegationStep =
  | "review"
  | "signing"
  | "waiting"
  | "takingLonger"
  | "confirmed"
  | "notConfirmed";

/** How long "waiting" lasts before the dialog says it is taking longer. */
const TAKING_LONGER_AFTER_MS = 60_000;

type Props = {
  /** The DRep to delegate to; the dialog is closed while this is undefined. */
  dRep?: Pick<DRepData, "drepId" | "view" | "givenName" | "isScriptBased">;
  onClose: () => void;
  /** Builds, signs and submits the delegation (useDelegateTodRep's delegate). */
  onConfirm: () => Promise<void>;
  onViewDRep?: () => void;
};

/**
 * Where the dialog is, from what it started and what the pending-transaction
 * context and the current delegation now say.
 */
export const nextDelegationStep = ({
  step,
  isDelegating,
  pendingResourceId,
  pendingTxHash,
  isNowDelegated,
  waitedMs = 0,
  dRep,
}: {
  step: DelegationStep;
  isDelegating: boolean;
  pendingResourceId?: string;
  pendingTxHash?: string;
  /** The wallet's delegation, as read once this transaction settled. */
  isNowDelegated: boolean;
  /** Time spent in "waiting". */
  waitedMs?: number;
  dRep: Pick<DRepData, "drepId" | "view">;
}): DelegationStep | "close" => {
  const isPendingForThis =
    !!pendingTxHash && isSameDRep(dRep as DRepData, pendingResourceId);
  if (step === "signing") {
    if (isPendingForThis) return "waiting";
    // The wallet refused or the build failed: the hook shows its error modal.
    if (!isDelegating) return "close";
  }
  if (step === "waiting" || step === "takingLonger") {
    // The pending transaction clears once its effect shows up, or when the
    // app gives up on it after a few minutes and reports it failed.
    if (!isPendingForThis) return isNowDelegated ? "confirmed" : "notConfirmed";
    if (step === "waiting" && waitedMs > TAKING_LONGER_AFTER_MS) return "takingLonger";
  }
  return step;
};

const Row = ({ label, value, isStrong }: { label: string; value: string; isStrong?: boolean }) => (
  <Box sx={{ alignItems: "baseline", display: "flex", gap: 2, justifyContent: "space-between" }}>
    <Box component="span" sx={{ color: ds.gray["600"], fontSize: 14 }}>
      {label}
    </Box>
    <Box component="span" sx={{ color: ds.gray["900"], fontSize: isStrong ? 16 : 14, fontWeight: 600, textAlign: "right", wordBreak: "break-all" }}>
      {value}
    </Box>
  </Box>
);

const Progress = ({ isConfirmed }: { isConfirmed: boolean }) => {
  const { t } = useTranslation();
  const steps = [
    { label: t("delegationReview.progress.signed"), done: true },
    { label: t("delegationReview.progress.submitted"), done: true },
    { label: t("delegationReview.progress.confirming"), done: isConfirmed },
  ];
  return (
    <Box sx={{ backgroundColor: ds.gray["25"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "flex", flexDirection: "column", gap: 1.25, p: 2 }}>
      <Box sx={{ color: ds.gray["600"], fontSize: 11, fontWeight: 600, letterSpacing: "0.3px", textTransform: "uppercase" }}>
        {t("delegationReview.progress.title")}
      </Box>
      {steps.map((item) => (
        <Box key={item.label} sx={{ alignItems: "center", color: item.done ? ds.success["700"] : ds.gray["700"], display: "flex", fontSize: 14, gap: 1 }}>
          {item.done ? <Check aria-hidden size={16} /> : <CircularProgress size={14} sx={{ color: ds.gray["500"] }} thickness={5} />}
          {item.label}
        </Box>
      ))}
    </Box>
  );
};

/**
 * Review voting delegation before the wallet asks to sign, then follow the
 * submitted transaction until Cardano confirms it.
 */
export const DelegationReviewDialog = ({ dRep, onClose, onConfirm, onViewDRep }: Props) => {
  const { t } = useTranslation();
  const { stakeKey, pendingTransaction, isStakeKeyRegistered } = useCardano();
  const { networkName, epochParams, cExplorerBaseUrl } = useAppContext();
  const { addSuccessAlert } = useSnackbar();
  const queryClient = useQueryClient();
  const { voter } = useGetVoterInfo();
  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const [step, setStep] = useState<DelegationStep>("review");
  const [isDelegating, setIsDelegating] = useState(false);
  const [txHash, setTxHash] = useState<string>();
  const [waitingSince, setWaitingSince] = useState<number>();
  const [now, setNow] = useState(() => Date.now());

  const pending = pendingTransaction.delegate;
  // While this transaction is pending the delegation query is keyed by its
  // hash, and the pending-transaction context refetches that entry until it
  // shows the change. Read that entry, not the stale unkeyed one.
  const settledDelegation = txHash
    ? queryClient.getQueryData<CurrentDelegation>([
        QUERY_KEYS.getAdaHolderCurrentDelegationKey,
        txHash,
      ])
    : undefined;
  const delegation = settledDelegation ?? currentDelegation;
  const isNowDelegated =
    !!dRep &&
    (isSameDRep(dRep as DRepData, delegation?.dRepHash) ||
      isSameDRep(dRep as DRepData, delegation?.dRepView));

  useEffect(() => {
    if (!dRep) {
      setStep("review");
      setTxHash(undefined);
      setWaitingSince(undefined);
    }
  }, [dRep]);

  useEffect(() => {
    if (step !== "waiting") return undefined;
    setWaitingSince((since) => since ?? Date.now());
    const timer = setInterval(() => setNow(Date.now()), 5_000);
    return () => clearInterval(timer);
  }, [step]);

  useEffect(() => {
    if (!dRep) return;
    if (pending?.transactionHash) setTxHash(pending.transactionHash);
    const next = nextDelegationStep({
      step,
      isDelegating,
      pendingResourceId: pending?.resourceId,
      pendingTxHash: pending?.transactionHash,
      isNowDelegated,
      waitedMs: waitingSince ? now - waitingSince : 0,
      dRep,
    });
    if (next === "close") onClose();
    else if (next !== step) setStep(next);
  }, [
    dRep,
    step,
    isDelegating,
    pending?.resourceId,
    pending?.transactionHash,
    isNowDelegated,
    now,
    waitingSince,
  ]);

  if (!dRep) return null;

  const name = dRep.givenName || `${dRep.view.slice(0, 12)}…${dRep.view.slice(-6)}`;
  const cip129Id = encodeCIP129Identifier({
    txID: `${dRep.isScriptBased ? "23" : "22"}${dRep.drepId}`,
    bech32Prefix: "drep",
  });
  const walletName = getItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
  const power =
    votingPower === undefined
      ? "—"
      : `₳\u00a0${correctDRepDirectoryFormat(votingPower)}`;
  // The hook adds a stake key registration when the key is not registered,
  // which takes a refundable deposit.
  const keyDeposit =
    !isStakeKeyRegistered() && epochParams?.key_deposit
      ? correctAdaFormat(epochParams.key_deposit)
      : undefined;
  const isDirectVoter = !!voter?.isRegisteredAsSoleVoter;

  const confirm = async () => {
    setStep("signing");
    setIsDelegating(true);
    try {
      await onConfirm();
    } finally {
      setIsDelegating(false);
    }
  };

  const heading = {
    review: t("delegationReview.title"),
    signing: t("delegationReview.signingTitle"),
    waiting: t("delegationReview.waitingTitle"),
    takingLonger: t("delegationReview.takingLongerTitle"),
    confirmed: t("delegationReview.confirmedTitle"),
    notConfirmed: t("delegationReview.notConfirmedTitle"),
  }[step];

  return (
    <Dialog
      aria-labelledby="delegation-review-title"
      data-testid="delegation-review-dialog"
      onClose={onClose}
      open
      PaperProps={{ sx: { borderRadius: `${RADIUS.xl}px`, maxWidth: 520, p: { xxs: 2.5, md: 4 }, width: "100%" } }}
    >
      <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
        <Box sx={{ alignItems: "flex-start", display: "flex", gap: 2, justifyContent: "space-between" }}>
          <Box
            aria-live="polite"
            role="status"
            sx={{ alignItems: step === "review" ? "flex-start" : "center", display: "flex", flex: 1, flexDirection: "column", gap: 1, textAlign: step === "review" ? "left" : "center" }}
          >
            {(step === "signing" || step === "waiting" || step === "takingLonger") && (
              <CircularProgress size={32} sx={{ color: ds.orange["500"], mb: 1 }} />
            )}
            {step === "confirmed" && (
              <Box sx={{ alignItems: "center", backgroundColor: ds.success["50"], borderRadius: "50%", color: ds.success["600"], display: "flex", height: 56, justifyContent: "center", mb: 1, width: 56 }}>
                <CircleCheck aria-hidden size={28} />
              </Box>
            )}
            {step === "notConfirmed" && (
              <Box sx={{ alignItems: "center", backgroundColor: ds.warning["50"], borderRadius: "50%", color: ds.warning["600"], display: "flex", height: 56, justifyContent: "center", mb: 1, width: 56 }}>
                <TriangleAlert aria-hidden size={28} />
              </Box>
            )}
            <Box component="h2" id="delegation-review-title" sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, lineHeight: "30px", m: 0 }}>
              {heading}
            </Box>
            <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>
              {
                {
                  review: t("delegationReview.subtitle"),
                  signing: t("delegationReview.signingSubtitle"),
                  waiting: t("delegationReview.waitingSubtitle"),
                  takingLonger: t("delegationReview.takingLongerSubtitle"),
                  confirmed: t("delegationReview.confirmedSubtitle", { name }),
                  notConfirmed: t("delegationReview.notConfirmedSubtitle"),
                }[step]
              }
            </Box>
          </Box>
          <ButtonBase aria-label={t("close")} data-testid="delegation-review-close" onClick={onClose} sx={{ borderRadius: "50%", color: ds.gray["600"], height: 32, width: 32 }}>
            <X aria-hidden size={18} />
          </ButtonBase>
        </Box>

        {step === "review" && (
          <>
            <Box sx={{ alignItems: "center", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "flex", gap: 2, justifyContent: "space-between", p: 2 }}>
              <Box sx={{ minWidth: 0 }}>
                <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, textTransform: "capitalize" }}>
                  {t("delegationReview.wallet", { name: walletName ?? "" })}
                </Box>
                <Box sx={{ color: ds.gray["600"], fontSize: 13, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                  {stakeKey}
                </Box>
              </Box>
              <Box sx={{ color: ds.primary["600"], fontSize: 13, fontWeight: 500, flexShrink: 0, textTransform: "capitalize" }}>
                {networkName}
              </Box>
            </Box>
            <Box sx={{ backgroundColor: ds.gray["25"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "flex", flexDirection: "column", gap: 1.5, p: 2.5 }}>
              <Box sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600 }}>{t("delegationReview.summary")}</Box>
              <Box sx={{ borderTop: `1px dashed ${ds.gray["300"]}` }} />
              <Row label={t("delegationReview.destination")} value={name} />
              <Row label={t("delegationReview.dRepId")} value={`${cip129Id.slice(0, 14)}…${cip129Id.slice(-6)}`} />
              <Row isStrong label={t("delegationReview.votingPower")} value={power} />
              <Row
                label={t("delegationReview.adaTransferred")}
                value={keyDeposit ? t("delegationReview.keyDeposit", { amount: keyDeposit }) : t("delegationReview.none")}
              />
            </Box>
            {isDirectVoter && (
              <Box data-testid="delegation-review-direct-voter" sx={{ alignItems: "flex-start", backgroundColor: PILL_TONES.primary.bg, border: `1px solid ${PILL_TONES.primary.border}`, borderRadius: `${RADIUS.md}px`, color: PILL_TONES.primary.text, display: "flex", fontSize: 14, gap: 1, lineHeight: "20px", p: 2 }}>
                <Info aria-hidden size={18} style={{ flexShrink: 0, marginTop: 1 }} />
                {t("delegationReview.directVoterNote")}
              </Box>
            )}
            <Box sx={{ alignItems: "flex-start", backgroundColor: PILL_TONES.warning.bg, border: `1px solid ${PILL_TONES.warning.border}`, borderRadius: `${RADIUS.md}px`, color: PILL_TONES.warning.text, display: "flex", fontSize: 14, gap: 1, lineHeight: "20px", p: 2 }}>
              <TriangleAlert aria-hidden size={18} style={{ flexShrink: 0, marginTop: 1 }} />
              {t("delegationReview.note")}
            </Box>
          </>
        )}

        {(step === "waiting" || step === "takingLonger") && <Progress isConfirmed={false} />}

        {step === "confirmed" && (
          <Box sx={{ backgroundColor: ds.gray["25"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "flex", flexDirection: "column", gap: 1.5, p: 2.5 }}>
            {txHash && (
              <Box sx={{ display: "flex", flexDirection: "column", gap: 0.5 }}>
                <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{t("delegationReview.transactionId")}</Box>
                <ButtonBase
                  aria-label={t("delegationReview.copyTransactionId")}
                  onClick={() => {
                    navigator.clipboard.writeText(txHash);
                    addSuccessAlert(t("alerts.copiedToClipboard"));
                  }}
                  sx={{ color: ds.gray["600"], fontFamily: "inherit", fontSize: 13, gap: 0.75, justifyContent: "flex-start" }}
                >
                  <Box component="span" sx={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{txHash}</Box>
                  <Copy aria-hidden size={14} style={{ flexShrink: 0 }} />
                </ButtonBase>
              </Box>
            )}
            <Box sx={{ borderTop: `1px dashed ${ds.gray["300"]}` }} />
            <Row label={t("delegationReview.destination")} value={name} />
            <Row isStrong label={t("delegationReview.votingPower")} value={power} />
          </Box>
        )}

        {step === "notConfirmed" && txHash && (
          <Box
            component="a"
            data-testid="delegation-review-explorer-link"
            href={`${cExplorerBaseUrl}/tx/${txHash}`}
            rel="noopener noreferrer"
            sx={{ alignSelf: "center", color: ds.primary["600"], fontSize: 14, fontWeight: 600, textDecoration: "none", "&:hover": { textDecoration: "underline" } }}
            target="_blank"
          >
            {t("delegationReview.checkExplorer")}
          </Box>
        )}

        <Box sx={{ borderTop: `1px solid ${ds.gray["100"]}` }} />
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between" }}>
          {step === "review" && (
            <>
              <Button dataTestId="delegation-review-back" onClick={onClose} variant="outlined">
                {t("delegationReview.back")}
              </Button>
              <Button dataTestId="continue-to-wallet-button" onClick={confirm}>
                {t("delegationReview.continue")}
              </Button>
            </>
          )}
          {(step === "signing" || step === "waiting" || step === "takingLonger" || step === "notConfirmed") && (
            <Button dataTestId="delegation-review-hide" onClick={onClose} sx={{ ml: "auto" }} variant="outlined">
              {t("delegationReview.hide")}
            </Button>
          )}
          {step === "confirmed" && (
            <>
              <Button dataTestId="delegation-review-done" onClick={onClose} variant="outlined">
                {t("close")}
              </Button>
              {onViewDRep && (
                <Button dataTestId="delegation-review-view-drep" onClick={onViewDRep}>
                  {t("delegationReview.viewDRep")}
                </Button>
              )}
            </>
          )}
        </Box>
        {step === "review" && (
          <Box sx={{ color: ds.gray["500"], fontSize: 12, textAlign: "center" }}>
            {t("delegationReview.walletHint")}
          </Box>
        )}
      </Box>
    </Dialog>
  );
};
