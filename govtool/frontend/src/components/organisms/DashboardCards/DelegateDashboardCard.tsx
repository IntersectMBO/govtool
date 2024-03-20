import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import {
  CopyableInfo,
  DashboardActionCard,
  DashboardActionCardProps,
} from "@molecules";
import { correctAdaFormat, formHexToBech32, openInNewTab } from "@utils";
import { PendingTransaction } from "@/context/pendingTransaction";

type DelegateDashboardCardProps = {
  currentDelegation: string;
  delegateTx: PendingTransaction["delegate"];
  dRepID: string;
  votingPower: number;
};

export const DelegateDashboardCard = ({
  currentDelegation,
  delegateTx,
  dRepID,
  votingPower,
}: DelegateDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const ada = correctAdaFormat(votingPower);

  const cardProps: Partial<DashboardActionCardProps> = (() => {
    // transaction in progress
    if (delegateTx) {
      return {
        buttons: [
          {
            children: t("seeTransaction"),
            dataTestId: "see-transaction-button",
            onClick: () =>
              openInNewTab("https://adanordic.com/latest_transactions"),
          },
        ],
        description: getProgressDescription(
          delegateTx?.resourceId,
          dRepID,
          ada
        ),
        state: "inProgress",
        title: t("dashboard.delegation.votingPowerDelegation"),
      };
    }

    // current delegation
    if (currentDelegation) {
      return {
        buttons: [
          {
            children: t("dashboard.delegation.changeDelegation"),
            dataTestId: "change-dRep-button",
            onClick: () => navigate(PATHS.delegateTodRep),
          },
        ],
        description: getDelegationDescription(currentDelegation, dRepID, ada),
        state: "active",
        title: (
          <Trans i18nKey="dashboard.delegation.yourVotingPowerIsDelegated" />
        ),
      };
    }

    // no current delegation
    return {
      buttons: [
        {
          children: t("delegate"),
          dataTestId: "delegate-button",
          onClick: () => navigate(PATHS.delegateTodRep),
          variant: "contained",
        },
        {
          children: t("learnMore"),
          dataTestId: "delegate-learn-more-button",
          onClick: () =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power"
            ),
        },
      ],
      description: (
        <Trans
          i18nKey="dashboard.delegation.delegateOwnPower"
          values={{ ada }}
        />
      ),
      title: t("dashboard.delegation.useYourVotingPower"),
    };
  })();

  const displayedDelegationId = getDisplayedDelegationId(
    currentDelegation,
    delegateTx?.resourceId,
    dRepID
  );

  return (
    <DashboardActionCard
      imageURL={IMAGES.govActionDelegateImage}
      {...cardProps}
    >
      {displayedDelegationId && (
        <CopyableInfo
          dataTestId="delegated-to-drep-id"
          label={t("dashboard.delegation.dRepDelegatedTo")}
          value={displayedDelegationId}
        />
      )}
    </DashboardActionCard>
  );
};

const getDelegationDescription = (
  currentDelegation: string,
  dRepID: string,
  ada: number
) => {
  const key =
    currentDelegation === dRepID
      ? "dashboard.delegation.toYourself"
      : currentDelegation === "drep_always_no_confidence"
        ? "dashboard.delegation.voteNo"
        : currentDelegation === "drep_always_abstain"
          ? "dashboard.delegation.voteAbstain"
          : currentDelegation
            ? "dashboard.delegation.toDRep"
            : undefined;
  return <Trans i18nKey={key} values={{ ada }} />;
};

const getProgressDescription = (
  delegateTo: string,
  dRepID: string,
  ada: number
) => {
  const key = (() => {
    if (!delegateTo) return undefined;
    switch (delegateTo) {
      case dRepID:
        return "dashboard.delegation.inProgress.toYourself";
      case "no confidence":
        return "dashboard.delegation.inProgress.voteNo";
      case "abstain":
        return "dashboard.delegation.inProgress.voteAbstain";
      default:
          return "dashboard.delegation.inProgress.toDRep";
    }
  })();
  return <Trans i18nKey={key} values={{ ada }} />;
};

const getDisplayedDelegationId = (
  currentDelegation: string,
  delegateTo: string | undefined,
  dRepID: string
) => {
  const restrictedNames = [
    dRepID,
    "drep_always_abstain",
    "drep_always_no_confidence",
    "abstain",
    "no confidence",
  ];
  if (delegateTo) {
    if (!restrictedNames.includes(delegateTo)) {
      return delegateTo.includes("drep")
        ? delegateTo
        : formHexToBech32(delegateTo);
    }
    return undefined;
  }
  if (!restrictedNames.includes(currentDelegation)) {
    return formHexToBech32(currentDelegation);
  }
  return undefined;
};
