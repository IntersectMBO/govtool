import { Dispatch, ReactNode, SetStateAction, useEffect } from "react";
import { Box, ButtonBase } from "@mui/material";
import { Download, ExternalLink, Wallet } from "lucide-react";

import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";
import { ControlledField } from "@organisms";
import {
  URL_REGEX,
  ellipsizeText,
  isValidURLLength,
  openInNewTab,
} from "@utils";
import { LINKS } from "@/consts/links";
import { ds, PILL_TONES, RADIUS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { useCardano } from "@/context/wallet";
import { correctVoteAdaFormat } from "@/utils/adaFormat";
import { connectedWallet } from "@/utils/connectedWallet";
import { stakeHexToBech32 } from "@/utils/drepDashboard";
import { networkLabel } from "@/utils/walletConnect";

import { InfoNote, SectionCard, StepHeading } from "./parts";

type StorageInformationProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

const short = (value: string) => `${value.slice(0, 10)}…${value.slice(-6)}`;

const focusRing = {
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

type StorageStepProps = { number: number; label: string; children?: ReactNode };

const StorageStep = ({ number, label, children }: StorageStepProps) => (
  <Box component="li" sx={{ display: "flex", gap: 1.5 }}>
    <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: "50%", color: ds.primary["700"], display: "flex", flexShrink: 0, fontSize: 13, fontWeight: 600, height: 26, justifyContent: "center", width: 26 }}>
      {number}
    </Box>
    <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 1.25, minWidth: 0, pt: 0.25 }}>
      <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 500 }}>{label}</Box>
      {children}
    </Box>
  </Box>
);

export const StorageInformation = ({ setStep }: StorageInformationProps) => {
  const { t } = useTranslation();
  const {
    control,
    errors,
    createGovernanceAction,
    getValues,
    watch,
    generateMetadata,
    onClickDownloadJson,
    isLoading,
  } = useCreateGovernanceActionForm(setStep);
  const { epochParams, network } = useAppContext();
  const { stakeKey } = useCardano();
  const wallet = connectedWallet();

  const type = getValues("governance_action_type");
  const title = getValues("title");
  const fileName = (type ?? "") as string;
  const deposit = epochParams?.gov_action_deposit
    ? `₳${correctVoteAdaFormat(epochParams.gov_action_deposit)}`
    : "—";
  const address = stakeKey ? stakeHexToBech32(stakeKey) ?? stakeKey : undefined;

  const isActionButtonDisabled = !watch("storingURL") || !!errors.storingURL;

  useEffect(() => {
    generateMetadata();
  }, []);

  const detailRow = (label: string, value: string, testId: string) => (
    <Box sx={{ alignItems: "baseline", display: "flex", gap: 2, justifyContent: "space-between" }}>
      <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>{label}</Box>
      <Box data-testid={testId} sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, textAlign: "right" }}>{value}</Box>
    </Box>
  );
  const subheading = (text: string) => (
    <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0, mb: -1 }}>{text}</Box>
  );

  return (
    <>
      <StepHeading body={t("createGA.confirm.body")} title={t("createGA.confirm.title")} />
      <Box data-testid="create-ga-summary" sx={{ backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, px: 2.5, py: 2 }}>
        <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>
          {t("createGA.confirm.typeLabel")}{" "}
          <Box component="strong" sx={{ color: ds.gray["900"], fontWeight: 600 }}>
            {type ? t(`createGA.type.names.${type}`) : ""}
          </Box>
        </Box>
        {title && (
          <Box sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, mt: 1, wordBreak: "break-word" }}>{title}</Box>
        )}
      </Box>
      <SectionCard
        subtitle={t("createGovernanceAction.storingInformationDescription")}
        testId="create-ga-storage"
        title={t("createGA.confirm.storageTitle")}
      >
        <Box component="ol" sx={{ display: "flex", flexDirection: "column", gap: 2.5, listStyle: "none", m: 0, p: 0 }}>
          <StorageStep label={t("createGovernanceAction.storingInformationStep1Label")} number={1}>
            <ButtonBase
              data-testid="metadata-download-button"
              onClick={onClickDownloadJson}
              sx={{ alignSelf: "flex-start", border: `1px solid ${ds.gray["300"]}`, borderRadius: `${RADIUS.pill}px`, color: ds.gray["800"], fontFamily: "inherit", fontSize: 14, fontWeight: 500, gap: 1, height: 40, px: 2, "&:hover": { backgroundColor: ds.gray["50"] }, ...focusRing }}
            >
              <Download aria-hidden size={16} />
              {`${ellipsizeText(fileName, 8)}.jsonld`}
            </ButtonBase>
          </StorageStep>
          <StorageStep label={t("createGovernanceAction.storingInformationStep2Label")} number={2}>
            <ButtonBase
              data-testid="read-guide-button"
              onClick={() => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE)}
              sx={{ alignSelf: "flex-start", borderRadius: `${RADIUS.sm}px`, color: ds.primary["600"], fontFamily: "inherit", fontSize: 14, fontWeight: 500, gap: 0.75, "&:hover": { textDecoration: "underline" }, ...focusRing }}
            >
              {t("createGovernanceAction.storingInformationStep2Link")}
              <ExternalLink aria-hidden size={14} />
            </ButtonBase>
          </StorageStep>
          <StorageStep label={t("createGovernanceAction.storingInformationStep3Label")} number={3}>
            <ControlledField.Input
              {...{ control, errors }}
              dataTestId="metadata-url-input"
              name="storingURL"
              placeholder={t("createGovernanceAction.storingInformationURLPlaceholder")}
              rules={{
                required: {
                  value: true,
                  message: t("createGovernanceAction.fields.validations.required"),
                },
                pattern: {
                  value: URL_REGEX,
                  message: t("createGovernanceAction.fields.validations.url"),
                },
                validate: isValidURLLength,
              }}
            />
          </StorageStep>
        </Box>
      </SectionCard>
      {subheading(t("createGA.confirm.transaction"))}
      <Box data-testid="create-ga-transaction-details" sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", flexDirection: "column", gap: 1.5, px: 3, py: 2.25 }}>
        {detailRow(t("createGA.confirm.deposit"), deposit, "create-ga-deposit")}
        <Box aria-hidden sx={{ borderTop: `1px solid ${ds.gray["100"]}` }} />
        {detailRow(t("createGA.confirm.network"), networkLabel(network), "create-ga-network")}
      </Box>
      {subheading(t("createGA.confirm.wallet"))}
      <Box data-testid="create-ga-wallet" sx={{ alignItems: "center", backgroundColor: ds.primary["25"], border: `1px solid ${ds.primary["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", gap: 1.5, px: 2, py: 1.5 }}>
        <Box aria-hidden sx={{ alignItems: "center", backgroundColor: wallet?.icon ? "#FFFFFF" : ds.primary["600"], borderRadius: "50%", color: "#FFFFFF", display: "flex", flexShrink: 0, height: 36, justifyContent: "center", overflow: "hidden", width: 36 }}>
          {wallet?.icon ? (
            <Box alt="" component="img" src={wallet.icon} sx={{ height: 24, objectFit: "contain", width: 24 }} />
          ) : (
            <Wallet size={18} />
          )}
        </Box>
        <Box sx={{ flex: 1, minWidth: 0 }}>
          <Box sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600 }}>{wallet?.name ?? t("createGA.confirm.wallet")}</Box>
          {address && <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{short(address)}</Box>}
        </Box>
        <Box component="span" sx={{ backgroundColor: PILL_TONES.success.bg, borderRadius: `${RADIUS.xs}px`, color: PILL_TONES.success.text, fontSize: 11, fontWeight: 600, letterSpacing: "0.3px", px: 0.75, py: 0.25, textTransform: "uppercase" }}>
          {t("createGA.confirm.connected")}
        </Box>
      </Box>
      <InfoNote title={t("createGA.confirm.signatureTitle")}>{t("createGA.confirm.signatureBody")}</InfoNote>
      <CenteredBoxBottomButtons
        actionButtonText={t("createGA.confirm.submit")}
        backButtonText={t("createGA.confirm.edit")}
        disableActionButton={isActionButtonDisabled}
        isLoadingActionButton={isLoading}
        onActionButton={createGovernanceAction}
        onBackButton={() => setStep(3)}
      />
    </>
  );
};
