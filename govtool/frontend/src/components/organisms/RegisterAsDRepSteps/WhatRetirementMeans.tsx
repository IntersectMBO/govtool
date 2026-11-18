import { ReactNode, useCallback, useState } from "react";
import * as Sentry from "@sentry/react";
import { Box, ButtonBase, Checkbox, CircularProgress } from "@mui/material";
import { useTranslation } from "react-i18next";
import { CircleX, Info, TriangleAlert, Undo2, type LucideIcon } from "lucide-react";

import { ds, RADIUS } from "@/consts/designTokens";
import { useCardano } from "@/context/wallet";
import { useGetVoterInfo } from "@/hooks/queries/useGetVoterInfoQuery";
import { useWalletErrorModal } from "@/hooks/useWalletErrorModal";
import { correctVoteAdaFormat } from "@/utils/adaFormat";

const POINTS: { key: "voting" | "delegation" | "id"; icon: LucideIcon }[] = [
  { key: "voting", icon: CircleX },
  { key: "delegation", icon: Undo2 },
  { key: "id", icon: Info },
];

const halfButton = {
  borderRadius: `${RADIUS.pill}px`,
  flex: 1,
  fontSize: 15,
  fontWeight: 600,
  height: 44,
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

const Ack = ({
  checked,
  onChange,
  testId,
  children,
}: {
  checked: boolean;
  onChange: (next: boolean) => void;
  testId: string;
  children: ReactNode;
}) => (
  <Box component="label" sx={{ alignItems: "flex-start", color: ds.gray["700"], cursor: "pointer", display: "flex", fontSize: 14, gap: 1, lineHeight: "20px" }}>
    <Checkbox
      checked={checked}
      inputProps={{ "data-testid": testId } as object}
      onChange={(event) => onChange(event.target.checked)}
      sx={{ mt: -0.75, p: 0.75 }}
    />
    <span>{children}</span>
  </Box>
);

/** What the page needs to follow a retirement after this form unmounts. */
export type RetirementSubmission = {
  txHash: string;
  name?: string | null;
  /** ADA, formatted. */
  deposit?: string;
};

export const WhatRetirementMeans = ({
  onClickCancel,
  onSubmitted,
}: {
  onClickCancel: () => void;
  /** The page shows the progress: this form unmounts while the voter reloads. */
  onSubmitted: (submission: RetirementSubmission) => void;
}) => {
  const {
    isPendingTransaction,
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
  } = useCardano();
  const { t } = useTranslation();
  const [isRetirementLoading, setIsRetirementLoading] =
    useState<boolean>(false);
  const [acks, setAcks] = useState({ voting: false, deposit: false });
  const { voter } = useGetVoterInfo();
  const openWalletErrorModal = useWalletErrorModal();

  const retireAsDrep = useCallback(async () => {
    try {
      setIsRetirementLoading(true);
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;
      if (!voter?.deposit) throw new Error(t("errors.appCannotGetDeposit"));

      const certBuilder = await buildDRepRetirementCert(
        voter?.deposit.toString(),
      );
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "retireAsDrep",
        voter,
      });
      if (result) {
        onSubmitted({
          txHash: result,
          name: voter?.givenName,
          deposit: correctVoteAdaFormat(voter.deposit),
        });
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (error: any) {
      Sentry.captureException(error);
      openWalletErrorModal({
        error: error?.message ? error.message : JSON.stringify(error),
        onSumbit: onClickCancel,
        buttonText: t("modals.common.goToDashboard"),
        dataTestId: "retirement-transaction-error-modal",
      });
    } finally {
      setIsRetirementLoading(false);
    }
  }, [
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    isPendingTransaction,
    onSubmitted,
    voter,
  ]);

  const deposit = voter?.deposit ? correctVoteAdaFormat(voter.deposit) : undefined;
  const canRetire = acks.voting && acks.deposit && !isRetirementLoading;

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
      <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.error["50"], borderRadius: "50%", color: ds.error["600"], display: "flex", height: 56, justifyContent: "center", width: 56 }}>
        <TriangleAlert size={24} />
      </Box>
      <Box>
        <Box component="h1" sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, m: 0 }}>
          {t("retirement.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 1.5 }}>
          {t("retirement.intro")}
        </Box>
      </Box>
      <Box component="ul" sx={{ backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", flexDirection: "column", gap: 2, listStyle: "none", m: 0, p: 2.5 }}>
        {POINTS.map(({ key, icon: Icon }) => (
          <Box key={key} component="li" sx={{ display: "flex", gap: 1.5 }}>
            <Icon aria-hidden color={ds.gray["800"]} size={18} style={{ flexShrink: 0, marginTop: 1 }} />
            <Box>
              <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{t(`retirement.points.${key}.title`)}</Box>
              <Box sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", mt: 0.5 }}>{t(`retirement.points.${key}.body`)}</Box>
            </Box>
          </Box>
        ))}
      </Box>
      <Ack checked={acks.voting} onChange={(voting) => setAcks((current) => ({ ...current, voting }))} testId="retirement-ack-voting">
        {t("retirement.ackVoting")}
      </Ack>
      <Ack checked={acks.deposit} onChange={(next) => setAcks((current) => ({ ...current, deposit: next }))} testId="retirement-ack-deposit">
        {deposit ? t("retirement.ackDeposit", { deposit }) : t("retirement.ackDepositNoAmount")}
      </Ack>
      <Box aria-hidden sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />
      <Box sx={{ display: "flex", flexDirection: { xxs: "column-reverse", sm: "row" }, gap: 1.5 }}>
        <ButtonBase
          data-testid="cancel-retirement-button"
          onClick={onClickCancel}
          sx={{ ...halfButton, backgroundColor: ds.gray["100"], border: `1px solid ${ds.gray["200"]}`, color: ds.gray["800"] }}
        >
          {t("cancel")}
        </ButtonBase>
        <ButtonBase
          data-testid="continue-retirement-button"
          disabled={!canRetire}
          onClick={retireAsDrep}
          sx={{
            ...halfButton,
            backgroundColor: ds.orange["700"],
            color: "#FFFFFF",
            gap: 1,
            "&:hover": { backgroundColor: ds.orange["750"] },
            "&.Mui-disabled": { backgroundColor: ds.orange["200"], color: ds.orange["750"] },
          }}
        >
          {isRetirementLoading && <CircularProgress aria-hidden size={18} sx={{ color: "inherit" }} />}
          {t("retirement.continue")}
        </ButtonBase>
      </Box>
    </Box>
  );
};
