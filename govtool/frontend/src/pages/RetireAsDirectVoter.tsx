import { useCallback, useEffect, useRef, useState } from "react";
import { useNavigate } from "react-router";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase, CircularProgress } from "@mui/material";
import { CircleCheck, TriangleAlert } from "lucide-react";

import { CenteredBoxPageWrapper } from "@/components/molecules/CenteredBoxPageWrapper";
import { RetirementProgressDialog } from "@/components/organisms/RegisterAsDRepSteps/RetirementProgressDialog";
import { WrongRouteInfo } from "@/components/organisms/WrongRouteInfo";
import { PATHS } from "@/consts/paths";
import { ds, RADIUS } from "@/consts/designTokens";
import { useCardano } from "@/context/wallet";
import { useGetVoterInfo } from "@/hooks/queries/useGetVoterInfoQuery";
import { useWalletErrorModal } from "@/hooks/useWalletErrorModal";
import { correctVoteAdaFormat } from "@/utils/adaFormat";
import { checkIsWalletConnected } from "@/utils/checkIsWalletConnected";

const halfButton = {
  borderRadius: `${RADIUS.pill}px`,
  flex: 1,
  fontSize: 15,
  fontWeight: 600,
  height: 44,
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

type Submission = { txHash: string; deposit?: string };

export const RetireAsDirectVoter = () => {
  const navigate = useNavigate();
  const [isLoading, setIsLoading] = useState(false);
  const [submission, setSubmission] = useState<Submission | null>(null);
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();
  const openWalletErrorModal = useWalletErrorModal();
  const { buildSignSubmitConwayCertTx, buildDRepRetirementCert, pendingTransaction } = useCardano();

  const onRetire = useCallback(async () => {
    try {
      setIsLoading(true);
      if (!voter?.deposit) {
        throw new Error(t("errors.appCannotGetDeposit"));
      }
      const certBuilder = await buildDRepRetirementCert(
        voter?.deposit?.toString(),
      );
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "retireAsDirectVoter",
        voter,
      });
      if (result) {
        setSubmission({ txHash: result, deposit: correctVoteAdaFormat(voter.deposit) });
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (error: any) {
      openWalletErrorModal({
        error: error?.message ? error.message : JSON.stringify(error),
        buttonText: t("modals.common.goToDashboard"),
        onSumbit: () => navigate(PATHS.dashboard),
        dataTestId: "retirement-transaction-error-modal",
      });
    } finally {
      setIsLoading(false);
    }
  }, [buildDRepRetirementCert, buildSignSubmitConwayCertTx, voter]);

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  // The progress lives here: the pending hash is part of the voter query's
  // key, so the voter reloads as soon as the wallet submits. It also shows a
  // retirement still pending after a reload.
  const pendingHash = pendingTransaction?.retireAsDirectVoter?.transactionHash;
  const txHash = submission?.txHash ?? pendingHash;
  const sawPending = useRef(false);
  useEffect(() => {
    if (pendingHash) sawPending.current = true;
    // Only a voter seen retired earns the success banner; a pending state that
    // clears otherwise (expired, failed) goes to the dashboard without it.
    const isRetired =
      !!voter && !voter.isRegisteredAsSoleVoter && (!!submission || !!pendingHash);
    const isSettled = sawPending.current && !pendingHash;
    if (isRetired) {
      navigate(PATHS.dashboard, { replace: true, state: { retired: "directVoter" } });
    } else if (isSettled) {
      navigate(PATHS.dashboard, { replace: true });
    }
  }, [pendingHash, submission, voter]);

  const pageTitle = t("directVoter.retireDirectVoter");
  const deposit = voter?.deposit ? correctVoteAdaFormat(voter.deposit) : undefined;

  const form = (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
      <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.error["50"], borderRadius: "50%", color: ds.error["600"], display: "flex", height: 56, justifyContent: "center", width: 56 }}>
        <TriangleAlert size={24} />
      </Box>
      <Box>
        <Box component="h1" sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, m: 0 }}>{t("directVoter.retireTitle")}</Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 1.5 }}>{t("directVoter.retireIntro")}</Box>
      </Box>
      <Box component="ul" sx={{ backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", flexDirection: "column", gap: 2, listStyle: "none", m: 0, p: 2.5 }}>
        {[
          t("directVoter.retirePoints.abstain"),
          t("directVoter.retirePoints.again"),
          deposit ? t("directVoter.retirePoints.deposit", { deposit }) : t("directVoter.retirePoints.depositNoAmount"),
        ].map((point) => (
          <Box key={point} component="li" sx={{ color: ds.gray["600"], display: "flex", fontSize: 14, gap: 1.5, lineHeight: "20px" }}>
            <CircleCheck aria-hidden color={ds.gray["800"]} size={18} style={{ flexShrink: 0, marginTop: 1 }} />
            {point}
          </Box>
        ))}
      </Box>
      <Box aria-hidden sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />
      <Box sx={{ display: "flex", flexDirection: { xxs: "column-reverse", sm: "row" }, gap: 1.5 }}>
        <ButtonBase
          data-testid="cancel-retirement-button"
          onClick={() => navigate(PATHS.dashboard)}
          sx={{ ...halfButton, backgroundColor: ds.gray["100"], border: `1px solid ${ds.gray["200"]}`, color: ds.gray["800"] }}
        >
          {t("cancel")}
        </ButtonBase>
        <ButtonBase
          data-testid="continue-retirement-button"
          disabled={isLoading}
          onClick={onRetire}
          sx={{ ...halfButton, backgroundColor: ds.orange["700"], color: "#FFFFFF", gap: 1, "&:hover": { backgroundColor: ds.orange["750"] }, "&.Mui-disabled": { opacity: 0.7 } }}
        >
          {isLoading && <CircularProgress aria-hidden size={18} sx={{ color: "inherit" }} />}
          {t("directVoter.retireContinue")}
        </ButtonBase>
      </Box>
    </Box>
  );

  if (txHash) {
    const progressDeposit = submission?.deposit ?? deposit;
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} hideBox={!voter?.isRegisteredAsSoleVoter}>
        {voter?.isRegisteredAsSoleVoter && form}
        <RetirementProgressDialog
          bodyText={progressDeposit
            ? t("directVoter.progressBody", { deposit: progressDeposit })
            : t("directVoter.progressBodyNoAmount")}
          onClose={() => navigate(PATHS.dashboard)}
          showActions={false}
          title={t("directVoter.progressTitle")}
          txHash={txHash}
        />
      </CenteredBoxPageWrapper>
    );
  }

  if (!voter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} hideBox>
        <CircularProgress />
      </CenteredBoxPageWrapper>
    );

  if (!voter.isRegisteredAsSoleVoter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle}>
        <WrongRouteInfo
          title={t("directVoter.notDirectVoter.title")}
          description={t("directVoter.notDirectVoter.description")}
        />
      </CenteredBoxPageWrapper>
    );

  return <CenteredBoxPageWrapper pageTitle={pageTitle}>{form}</CenteredBoxPageWrapper>;
};
