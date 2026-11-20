import { forwardRef } from "react";
import { Box, ButtonBase, CircularProgress } from "@mui/material";
import { useNavigate } from "react-router";
import { useTranslation } from "react-i18next";
import {
  Ban,
  Check,
  CircleCheck,
  CircleDashed,
  CircleX,
  Globe,
  Landmark,
  LoaderCircle,
  Network,
  RefreshCw,
  Unplug,
  X,
} from "lucide-react";

import { ModalWrapper } from "@/components/atoms/modal/ModalWrapper";
import { LINKS } from "@/consts/links";
import { PATHS } from "@/consts/paths";
import { ds, RADIUS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { useModal } from "@/context/modal";
import { useCardano } from "@/context/wallet";
import { useGetAdaHolderCurrentDelegationQuery } from "@/hooks/queries/useGetAdaHolderCurrentDelegationQuery";
import { useGetVoterInfo } from "@/hooks/queries/useGetVoterInfoQuery";
import { useStakeKeyVotingPowers } from "@/hooks/queries/useStakeKeyVotingPower";
import { useTimedOut } from "@/hooks/useTimedOut";
import { connectedWallet } from "@/utils/connectedWallet";
import { stakeHexToBech32 } from "@/utils/drepDashboard";
import { openInNewTab } from "@/utils/openInNewTab";
import { requestProfileMenu } from "@/utils/profileMenu";
import { classifyWalletError, networkLabel } from "@/utils/walletConnect";

import { Callout, PrimaryAction, SecondaryAction, WalletRow, WalletStateFrame } from "./walletParts";

export type WalletConnectionState =
  | { view: "checking" }
  | { view: "error"; error?: string; walletName?: string };

const ada = (lovelace?: number) =>
  (lovelace === undefined ? "" : (lovelace / 1_000_000).toLocaleString("en-US", { maximumFractionDigits: 0 }));

const short = (value: string) => (value.length > 22 ? `${value.slice(0, 10)}…${value.slice(-8)}` : value);

type StepState = "complete" | "inProgress" | "pending";

const CHECK_BUDGET_MS = 15_000;

const Step = ({ label, state }: { label: string; state: StepState }) => {
  const { t } = useTranslation();
  const icon = {
    complete: <CircleCheck aria-hidden color={ds.success["600"]} size={18} />,
    inProgress: <LoaderCircle aria-hidden color={ds.primary["600"]} size={18} />,
    pending: <CircleDashed aria-hidden color={ds.gray["400"]} size={18} />,
  }[state];
  const color = { complete: ds.success["700"], inProgress: ds.primary["600"], pending: ds.gray["600"] }[state];
  return (
    <Box component="li" sx={{ alignItems: "center", display: "flex", gap: 1.25, py: 0.75 }}>
      {icon}
      <Box sx={{ color: state === "pending" ? ds.gray["600"] : ds.gray["900"], flex: 1, fontSize: 14, fontWeight: state === "inProgress" ? 600 : 400 }}>{label}</Box>
      <Box sx={{ color, fontSize: 12, fontWeight: 500 }}>{t(`walletConnect.checking.${state}`)}</Box>
    </Box>
  );
};

/** Checking a freshly connected wallet, then its summary. */
const CheckingAndSuccess = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { closeModal } = useModal();
  const { network } = useAppContext();
  const { isEnabled, stakeKey, dRepID } = useCardano();
  // Neither the voter nor the delegation read reports failure here, so the
  // check stops waiting after a while and shows what it has.
  const isOutOfTime = useTimedOut(CHECK_BUDGET_MS);
  const { voter } = useGetVoterInfo();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { powers, isError } = useStakeKeyVotingPowers(stakeKey ? [stakeKey] : []);
  const wallet = connectedWallet();
  const address = stakeKey ? stakeHexToBech32(stakeKey) ?? stakeKey : "";
  const power = stakeKey ? powers[stakeKey] : undefined;
  const powerDone = power !== undefined || isError || isOutOfTime;
  const governanceDone =
    (!!voter && currentDelegation !== undefined) || !dRepID || isOutOfTime;
  const label = networkLabel(network);

  const walletRow = (
    <WalletRow detail={address ? short(address) : undefined} icon={wallet?.icon} name={t("dashboard.wallet.walletName", { name: wallet?.name ?? "" })} />
  );

  if (!(isEnabled && stakeKey && powerDone && governanceDone)) {
    const steps: StepState[] = [
      isEnabled ? "complete" : "inProgress",
      stakeKey ? "complete" : "pending",
      powerDone ? "complete" : stakeKey ? "inProgress" : "pending",
      governanceDone ? "complete" : powerDone ? "inProgress" : "pending",
    ];
    return (
      <Box sx={{ alignItems: "center", display: "flex", flexDirection: "column", gap: 2, textAlign: "center" }}>
        <CircularProgress aria-hidden size={36} sx={{ color: ds.orange["500"] }} />
        <Box component="h2" id="wallet-connection-title" sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, m: 0 }}>
          {t("walletConnect.checking.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>{t("walletConnect.checking.body")}</Box>
        <Box sx={{ alignSelf: "stretch", textAlign: "left" }}>{walletRow}</Box>
        <Box role="status" sx={{ alignSelf: "stretch", backgroundColor: ds.gray["25"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, px: 2, py: 1.5, textAlign: "left" }}>
          <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, mb: 0.5 }}>{t("walletConnect.checking.progress")}</Box>
          <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0 }}>
            {(["walletConnected", "stakeKey", "votingPower", "governance"] as const).map((key, index) => (
              <Step key={key} label={t(`walletConnect.checking.${key}`)} state={steps[index]} />
            ))}
          </Box>
        </Box>
      </Box>
    );
  }

  let governance = t("walletConnect.status.notDelegated");
  if (!voter || currentDelegation === undefined) governance = "–";
  else if (voter.isRegisteredAsDRep) governance = t("walletConnect.status.drep");
  else if (voter.isRegisteredAsSoleVoter) governance = t("walletConnect.status.directVoter");
  else if (currentDelegation?.dRepView) governance = t("walletConnect.status.delegated");

  const summary = [
    { key: "votingPower", icon: Landmark, value: power === undefined ? "–" : `₳ ${ada(power)}` },
    { key: "network", icon: Globe, value: label },
    { key: "governance", icon: CircleDashed, value: governance, accent: true },
  ];

  return (
    <Box data-testid="wallet-connected-summary" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Box sx={{ alignItems: "center", display: "flex", flexDirection: "column", gap: 1.25, textAlign: "center" }}>
        <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.success["50"], borderRadius: "50%", color: ds.success["700"], display: "flex", height: 56, justifyContent: "center", width: 56 }}>
          <Check size={26} />
        </Box>
        <Box component="h2" id="wallet-connection-title" sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, m: 0 }}>
          {t("walletConnect.success.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>
          {power === undefined
            ? t("walletConnect.success.bodyNoPower", { network: label })
            : t("walletConnect.success.body", { network: label, power: ada(power) })}
        </Box>
      </Box>
      {walletRow}
      <Box sx={{ backgroundColor: ds.primary["25"], border: `1px solid ${ds.primary["100"]}`, borderRadius: `${RADIUS.lg}px`, px: 2, py: 1.5 }}>
        <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("walletConnect.success.stakeKey")}</Box>
        <Box sx={{ color: ds.primary["600"], fontSize: 15, fontWeight: 600, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={address}>
          {address}
        </Box>
      </Box>
      <Box sx={{ backgroundColor: ds.gray["25"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, px: 2, py: 1.5 }}>
        <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between", mb: 1 }}>
          <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{t("walletConnect.success.summary")}</Box>
          <Box component="span" sx={{ backgroundColor: ds.success["50"], borderRadius: `${RADIUS.pill}px`, color: ds.success["700"], fontSize: 12, fontWeight: 600, px: 1 }}>
            {t("walletConnect.success.verified")}
          </Box>
        </Box>
        <Box component="dl" sx={{ m: 0 }}>
          {summary.map(({ key, icon: Icon, value, accent }) => (
            <Box key={key} sx={{ alignItems: "center", display: "flex", gap: 1.25, py: 0.75 }}>
              <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: `${RADIUS.sm}px`, color: ds.primary["600"], display: "flex", height: 28, justifyContent: "center", width: 28 }}>
                <Icon size={15} />
              </Box>
              <Box component="dt" sx={{ color: ds.gray["700"], flex: 1, fontSize: 14 }}>{t(`walletConnect.success.${key}`)}</Box>
              <Box component="dd" data-testid={`wallet-summary-${key}`} sx={{ color: accent ? ds.primary["600"] : ds.gray["900"], fontSize: 14, fontWeight: 600, m: 0 }}>{value}</Box>
            </Box>
          ))}
        </Box>
      </Box>
      <Box sx={{ borderTop: `1px dashed ${ds.gray["200"]}`, pt: 2 }}>
        <PrimaryAction
          onClick={() => {
            closeModal();
            if (!requestProfileMenu()) navigate(PATHS.dashboard);
          }}
          testId="view-your-profile-button"
        >
          <Box component="span" sx={{ flex: 1 }}>{t("walletConnect.success.viewProfile")}</Box>
        </PrimaryAction>
      </Box>
    </Box>
  );
};

/** Why a connection failed, and what to do about it. */
const ConnectionError = ({ error, walletName }: { error?: string; walletName?: string }) => {
  const { t } = useTranslation();
  const { openModal, closeModal } = useModal();
  const wallet = walletName ?? "";
  const kind = classifyWalletError(error);
  const chooseAnother = (
    <SecondaryAction onClick={() => openModal({ type: "chooseWallet" })} testId="choose-another-wallet-button">
      {t("walletConnect.chooseAnother")}
    </SecondaryAction>
  );
  const tryAgain = (
    <PrimaryAction onClick={() => openModal({ type: "chooseWallet" })} testId="try-again-connect-button">
      <RefreshCw aria-hidden size={15} />
      {t("walletConnect.tryAgain")}
    </PrimaryAction>
  );
  const message = (error ?? "").replace(/^Error:\s*/, "");

  if (kind.kind === "network") {
    return (
      <WalletStateFrame
        body={message}
        closeTestId="confirm-modal-button"
        footer={(
          <>
            <SecondaryAction onClick={closeModal}>{t("walletConnect.cancel")}</SecondaryAction>
            {tryAgain}
          </>
        )}
        icon={Network}
        onClose={closeModal}
        status={t("walletConnect.network.status")}
        title={t("walletConnect.network.title", { network: networkLabel(kind.requiredNetwork) })}
        titleId="wallet-connection-title"
        tone="error"
      >
        {wallet && <WalletRow name={wallet} tag={networkLabel(kind.walletNetwork)} tagTone="error" />}
        <Callout
          body={t("walletConnect.network.currentBody")}
          title={t("walletConnect.network.currentTitle", { network: networkLabel(kind.walletNetwork) })}
          tone="error"
        />
      </WalletStateFrame>
    );
  }

  if (kind.kind === "declined") {
    return (
      <WalletStateFrame
        body={t("walletConnect.declined.body")}
        closeTestId="confirm-modal-button"
        footer={<>{chooseAnother}{tryAgain}</>}
        icon={CircleX}
        onClose={closeModal}
        status={t("walletConnect.declined.status")}
        title={t("walletConnect.declined.title")}
        titleId="wallet-connection-title"
        tone="error"
      >
        {wallet && <WalletRow name={wallet} />}
        <Callout body={t("walletConnect.declined.nothingBody")} title={t("walletConnect.declined.nothingTitle")} tone="error" />
      </WalletStateFrame>
    );
  }

  if (kind.kind === "unsupported") {
    return (
      <WalletStateFrame
        body={t("walletConnect.unsupported.body")}
        closeTestId="confirm-modal-button"
        footer={(
          <>
            <SecondaryAction onClick={() => openInNewTab(LINKS.COMPATIBLE_WALLETS)}>{t("walletConnect.unsupported.viewSupported")}</SecondaryAction>
            <PrimaryAction onClick={() => openModal({ type: "chooseWallet" })} testId="choose-another-wallet-button">
              {t("walletConnect.chooseAnother")}
            </PrimaryAction>
          </>
        )}
        icon={Ban}
        onClose={closeModal}
        status={t("walletConnect.unsupported.status")}
        title={t("walletConnect.unsupported.title")}
        titleId="wallet-connection-title"
        tone="error"
      >
        {wallet && <WalletRow name={wallet} tag={t("walletConnect.unsupported.status")} tagTone="error" />}
        <Callout body={t("walletConnect.unsupported.chooseBody")} title={t("walletConnect.unsupported.chooseTitle")} tone="error" />
      </WalletStateFrame>
    );
  }

  return (
    <WalletStateFrame
      body={t("walletConnect.generic.body")}
      closeTestId="confirm-modal-button"
      footer={<>{chooseAnother}{tryAgain}</>}
      icon={Unplug}
      onClose={closeModal}
      status={t("walletConnect.generic.status")}
      title={t("walletConnect.generic.title")}
      titleId="wallet-connection-title"
      tone="error"
    >
      {wallet && <WalletRow name={wallet} />}
      <Callout body={message || t("walletConnect.generic.detailFallback")} title={t("walletConnect.generic.detailTitle")} tone="error" />
    </WalletStateFrame>
  );
};

/** The connection result: checking and summary, or the reason it failed. */
export const WalletConnectionModal = forwardRef<HTMLDivElement>((_, ref) => {
  const { t } = useTranslation();
  const { state, closeModal } = useModal<WalletConnectionState>();
  const isError = state?.view === "error";
  return (
    <ModalWrapper
      ref={ref}
      dataTestId={isError ? "wallet-connection-error-modal" : "wallet-connection-modal"}
      hideCloseButton
      onClose={closeModal}
      sx={{ maxWidth: 500, p: 3 }}
    >
      {!isError && (
        // The E2E login dismisses this with its confirm-modal-button click.
        <ButtonBase
          aria-label={t("walletConnect.close")}
          data-testid="confirm-modal-button"
          onClick={closeModal}
          sx={{ backgroundColor: ds.gray["50"], borderRadius: "50%", color: ds.gray["600"], height: 32, position: "absolute", right: 16, top: 16, width: 32, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
        >
          <X aria-hidden size={16} />
        </ButtonBase>
      )}
      <Box aria-labelledby="wallet-connection-title" aria-modal="true" role="dialog">
        {state?.view === "error" ? <ConnectionError error={state.error} walletName={state.walletName} /> : <CheckingAndSuccess />}
      </Box>
    </ModalWrapper>
  );
});
