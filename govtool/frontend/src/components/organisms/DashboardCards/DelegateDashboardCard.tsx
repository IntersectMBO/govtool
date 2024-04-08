import { useCallback } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import {
  DashboardActionCard,
  DashboardActionCardProps,
  DelegationAction,
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

  const learnMoreButton = {
    children: t("learnMore"),
    dataTestId: "delegate-learn-more-button",
    onClick: () =>
      openInNewTab(
        "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power",
      ),
  };

  const onClickDelegateToAnotherDRep = () =>
    navigate(PATHS.dashboardDRepDirectory);

  const ada = correctAdaFormat(votingPower);

  const cardProps: Partial<DashboardActionCardProps> = (() => {
    // transaction in progress
    if (delegateTx) {
      return {
        buttons: [learnMoreButton],
        description: getProgressDescription(delegateTx?.resourceId, ada),
        state: "inProgress",
        title: t("dashboard.cards.delegation.inProgress.title"),
      };
    }

    // current delegation
    if (currentDelegation) {
      return {
        buttons: [
          learnMoreButton,
          {
            children: t("dashboard.cards.delegation.delegateToAnotherDRep"),
            dataTestId: "delegate-to-another-drep-button",
            onClick: onClickDelegateToAnotherDRep,
          },
        ],
        description: getDelegationDescription(currentDelegation),
        state: "active",
        title: (
          <Trans
            i18nKey="dashboard.cards.delegation.delegationTitle"
            values={{ ada }}
          />
        ),
      };
    }

    // no current delegation
    return {
      buttons: [
        {
          children: t("dashboard.cards.delegation.noDelegationActionButton"),
          dataTestId: "delegate-button",
          onClick: () => navigate(PATHS.dashboardDRepDirectory),
          variant: "contained",
        },
        learnMoreButton,
      ],
      description: t("dashboard.cards.delegation.noDelegationDescription"),
      title: t("dashboard.cards.delegation.noDelegationTitle"),
    };
  })();

  const displayedDelegationId = getDisplayedDelegationId(
    currentDelegation,
    delegateTx?.resourceId,
    dRepID,
  );

  const navigateToDRepDetails = useCallback(
    () =>
      navigate(
        PATHS.dashboardDRepDirectoryDRep.replace(
          ":dRepId",
          displayedDelegationId || "",
        ),
      ),
    [displayedDelegationId],
  );

  return (
    <DashboardActionCard
      imageURL={IMAGES.govActionDelegateImage}
      isSpaceBetweenButtons
      transactionId={delegateTx?.resourceId}
      {...cardProps}
    >
      {displayedDelegationId && (
        <DelegationAction
          dRepId={
            formHexToBech32(delegateTx?.resourceId) || displayedDelegationId
          }
          onClickArrow={navigateToDRepDetails}
          sx={{ mt: 1.5 }}
        />
      )}
    </DashboardActionCard>
  );
};

const getDelegationDescription = (currentDelegation: string) => {
  const key =
    currentDelegation === "drep_always_no_confidence"
      ? "dashboard.cards.delegation.no"
      : currentDelegation === "drep_always_abstain"
      ? "dashboard.cards.delegation.abstain"
      : undefined;
  return <Trans i18nKey={key} />;
};

const getProgressDescription = (delegateTo: string, ada: number) => {
  const key = (() => {
    if (!delegateTo) return undefined;
    switch (delegateTo) {
      case "no confidence":
        return "dashboard.cards.delegation.inProgress.no";
      case "abstain":
        return "dashboard.cards.delegation.inProgress.abstain";
      default:
        return "dashboard.cards.delegation.inProgress.dRep";
    }
  })();
  return <Trans i18nKey={key} values={{ ada }} />;
};

const getDisplayedDelegationId = (
  currentDelegation: string,
  delegateTo: string | undefined,
  dRepID: string,
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
