import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { LoadingButtonProps } from "@atoms";
import { IMAGES, PATHS } from "@consts";
import { PendingTransaction } from "@context";
import { useTranslation } from "@hooks";
import { VoterInfo } from "@models";
import { DashboardActionCard, DashboardActionCardProps } from "@molecules";
import { correctAdaFormat, openInNewTab } from "@utils";

type SoleVoterDashboardCardProps = {
  pendingTransaction: PendingTransaction;
  voter: VoterInfo;
  votingPower: number;
};

export const SoleVoterDashboardCard = ({
  pendingTransaction,
  voter,
  votingPower,
}: SoleVoterDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const ada = correctAdaFormat(votingPower);

  // learn more button
  const learnMoreButton: LoadingButtonProps = {
    children: t("learnMore"),
    dataTestId: "learn-more-button",
    onClick: () =>
      openInNewTab(
        "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep",
      ),
  };

  const cardProps: Partial<DashboardActionCardProps> = (() => {
    // transaction in progress
    if (
      !!pendingTransaction.registerAsSoleVoter ||
      !!pendingTransaction.retireAsSoleVoter
    ) {
      return {
        buttons: [learnMoreButton],
        state: "inProgress",
        ...(pendingTransaction.registerAsSoleVoter && {
          description: t("dashboard.cards.soleVoter.registrationInProgress"),
          transactionId:
            pendingTransaction?.registerAsSoleVoter?.transactionHash,
          title: t("dashboard.cards.soleVoter.registration"),
        }),
        ...(pendingTransaction.retireAsSoleVoter && {
          description: t("dashboard.cards.soleVoter.retirementInProgress"),
          transactionId: pendingTransaction?.retireAsSoleVoter?.transactionHash,
          title: t("dashboard.cards.soleVoter.retirement"),
        }),
      };
    }

    // currently registered
    if (voter?.isRegisteredAsSoleVoter) {
      return {
        buttons: [
          {
            children: t("dashboard.cards.soleVoter.retire"),
            dataTestId: "retire-as-sole-voter-button",
            onClick: () => navigate(PATHS.retireAsSoleVoter),
            sx: { backgroundColor: "arcticWhite" },
          },
          { ...learnMoreButton, variant: "text" },
        ],
        description: (
          <Trans
            i18nKey="dashboard.cards.soleVoter.isRegisteredDescription"
            values={{ votingPower: ada }}
          />
        ),
        state: "active",
        transactionId: voter?.soleVoterRegisterTxHash,
        title: t("dashboard.cards.soleVoter.youAreSoleVoterTitle"),
      };
    }

    // was registered
    if (voter?.wasRegisteredAsSoleVoter) {
      return {
        buttons: [
          {
            children: t("dashboard.cards.soleVoter.reRegister"),
            dataTestId: "register-as-sole-voter-button",
            onClick: () => navigate(PATHS.registerAsSoleVoter),
            variant: "contained",
          },
          learnMoreButton,
        ],
        description: (
          <Trans
            i18nKey="dashboard.cards.soleVoter.wasRegisteredDescription"
            values={{ votingPower: ada }}
          />
        ),
        transactionId: voter?.soleVoterRetireTxHash,
        title: t("dashboard.cards.soleVoter.wasSoleVoterTitle"),
      };
    }

    // not registered
    return {
      buttons: [
        {
          children: t("dashboard.cards.soleVoter.register"),
          dataTestId: "register-as-sole-voter-button",
          onClick: () => navigate(PATHS.registerAsSoleVoter),
          variant: "contained",
        },
        learnMoreButton,
      ],
      description: (
        <Trans
          i18nKey="dashboard.cards.soleVoter.registerDescription"
          values={{ votingPower: ada }}
        />
      ),
      title: t("dashboard.cards.soleVoter.registerTitle"),
    };
  })();

  return (
    <DashboardActionCard imageURL={IMAGES.soleVoterImage} {...cardProps} />
  );
};
