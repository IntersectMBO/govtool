import { useCallback, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { PendingTransaction } from "@context";
import { useGetDRepDetailsQuery, useTranslation } from "@hooks";
import { CurrentDelegation, MetadataStandard, VoterInfo } from "@models";
import {
  DashboardActionCard,
  DashboardActionCardProps,
  DelegationAction,
} from "@molecules";
import {
  correctVoteAdaFormat,
  getMetadataDataMissingStatusTranslation,
  openInNewTab,
} from "@utils";
import {
  AutomatedVotingOptionCurrentDelegation,
  AutomatedVotingOptionDelegationId,
} from "@/types/automatedVotingOptions";
import { LINKS } from "@/consts/links";
import { useValidateMutation } from "@/hooks/mutations";

type DelegateDashboardCardProps = {
  currentDelegation: CurrentDelegation;
  delegateTx: PendingTransaction["delegate"];
  dRepID: string;
  voter: VoterInfo;
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

  const { dRep: myDRepDelegationData, isLoading } = useGetDRepDetailsQuery(
    delegateTx?.resourceId ?? currentDelegation?.dRepHash,
  );

  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const { validateMetadata } = useValidateMutation();

  useEffect(() => {
    if (!myDRepDelegationData?.url) return;

    const validate = async () => {
      const { status } = await validateMetadata({
        standard: MetadataStandard.CIP119,
        url: myDRepDelegationData.url!,
        hash: myDRepDelegationData?.metadataHash ?? "",
      });

      setMetadataStatus(status);
    };
    validate();
  }, [myDRepDelegationData?.url]);

  const learnMoreButton = {
    children: t("learnMore"),
    dataTestId: "delegate-learn-more-button",
    onClick: () => openInNewTab(LINKS.DELEGATING),
    sx: { backgroundColor: "arcticWhite" },
  };

  const displayedDelegationId = getDisplayedDelegationId({
    currentDelegation: currentDelegation?.dRepView,
    delegateTo: delegateTx?.resourceId,
    dRepID,
  });

  const onClickDelegateToAnotherDRep = () =>
    navigate(PATHS.dashboardDRepDirectory);

  const ada = correctVoteAdaFormat(votingPower);

  const cardProps: Partial<DashboardActionCardProps> = (() => {
    // transaction in progress
    if (delegateTx && delegateTx?.resourceId !== dRepID) {
      return {
        buttons: [learnMoreButton],
        description: getProgressDescription(delegateTx?.resourceId, ada),
        state: "inProgress",
        title: t("dashboard.cards.delegation.inProgress.title"),
      };
    }

    // current delegation
    if (
      !delegateTx &&
      currentDelegation &&
      currentDelegation?.dRepHash !== dRepID
    ) {
      return {
        buttons: currentDelegation?.dRepView
          ? [
              learnMoreButton,
              {
                children: t("dashboard.cards.delegation.delegateToAnotherDRep"),
                dataTestId: "delegate-to-another-drep-button",
                onClick: onClickDelegateToAnotherDRep,
                sx: { backgroundColor: "arcticWhite" },
              },
            ]
          : [learnMoreButton],
        description: getDelegationDescription(currentDelegation.dRepView),
        state: "active",
        title: getDelegationTitle(currentDelegation.dRepView, ada),
      };
    }

    // no current delegation
    return {
      buttons: [
        {
          children: t("dashboard.cards.delegation.noDelegationActionButton"),
          dataTestId: "view-drep-directory-button",
          onClick: () => navigate(PATHS.dashboardDRepDirectory),
          variant: "contained",
        },
        learnMoreButton,
      ],
      description: t("dashboard.cards.delegation.noDelegationDescription"),
      title: t("dashboard.cards.delegation.noDelegationTitle"),
    };
  })();

  const navigateToDRepDetails = useCallback(
    () =>
      navigate(
        PATHS.dashboardDRepDirectoryDRep.replace(
          ":dRepId",
          displayedDelegationId ?? "",
        ),
        { state: { enteredFromWithinApp: true } },
      ),
    [displayedDelegationId],
  );

  return (
    <DashboardActionCard
      imageURL={IMAGES.govActionDelegateImage}
      isSpaceBetweenButtons={
        !!currentDelegation?.dRepView &&
        !(currentDelegation?.dRepHash === dRepID)
      }
      transactionId={
        (delegateTx && delegateTx?.resourceId !== dRepID) ||
        (!delegateTx &&
          currentDelegation &&
          currentDelegation?.dRepHash !== dRepID)
          ? delegateTx?.transactionHash ?? currentDelegation?.txHash
          : undefined
      }
      type="delegate"
      {...cardProps}
    >
      {displayedDelegationId &&
      ((delegateTx && delegateTx?.resourceId !== dRepID) ||
        (!delegateTx &&
          currentDelegation &&
          currentDelegation?.dRepHash !== dRepID)) ? (
        // That rule is wrongly reporting an error on those lines
        // eslint-disable-next-line react/jsx-indent
        <DelegationAction
          drepName={
            isLoading
              ? "Loading..."
              : metadataStatus
              ? getMetadataDataMissingStatusTranslation(metadataStatus)
              : myDRepDelegationData?.givenName ?? ""
          }
          dRepId={displayedDelegationId}
          onCardClick={navigateToDRepDetails}
          sx={{ mt: 1.5 }}
        />
      ) : null}
    </DashboardActionCard>
  );
};

const getDelegationTitle = (currentDelegation: string | null, ada: string) => {
  const key =
    currentDelegation ===
    AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence
      ? "dashboard.cards.delegation.noConfidenceDelegationTitle"
      : currentDelegation ===
        AutomatedVotingOptionCurrentDelegation.drep_always_abstain
      ? "dashboard.cards.delegation.abstainDelegationTitle"
      : "dashboard.cards.delegation.dRepDelegationTitle";

  return <Trans i18nKey={key} values={{ ada }} />;
};

const getDelegationDescription = (currentDelegation: string | null) => {
  const key =
    currentDelegation ===
    AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence
      ? "dashboard.cards.delegation.noDescription"
      : currentDelegation ===
        AutomatedVotingOptionCurrentDelegation.drep_always_abstain
      ? "dashboard.cards.delegation.abstainDescription"
      : undefined;
  return <Trans i18nKey={key} />;
};

const getProgressDescription = (delegateTo: string, ada: string) => {
  const key = (() => {
    if (!delegateTo) return undefined;
    switch (delegateTo) {
      case AutomatedVotingOptionDelegationId.no_confidence:
        return "dashboard.cards.delegation.inProgress.no";
      case AutomatedVotingOptionDelegationId.abstain:
        return "dashboard.cards.delegation.inProgress.abstain";
      default:
        return "dashboard.cards.delegation.inProgress.dRep";
    }
  })();
  return <Trans i18nKey={key} values={{ ada }} />;
};

const getDisplayedDelegationId = ({
  dRepID,
  currentDelegation,
  delegateTo,
}: {
  dRepID: string;
  currentDelegation?: string | null;
  delegateTo?: string;
}) => {
  const restrictedNames = [
    dRepID,
    AutomatedVotingOptionCurrentDelegation.drep_always_abstain,
    AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence,
    AutomatedVotingOptionDelegationId.abstain,
    AutomatedVotingOptionDelegationId.no_confidence,
  ];
  if (delegateTo) {
    if (!restrictedNames.includes(delegateTo)) {
      return delegateTo;
    }
    return undefined;
  }

  if (!restrictedNames.includes(currentDelegation ?? "")) {
    return currentDelegation ?? "";
  }
  return undefined;
};
