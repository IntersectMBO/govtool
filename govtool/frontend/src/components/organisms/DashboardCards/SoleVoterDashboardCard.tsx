import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard, DashboardActionCardProps } from "@molecules";
import { correctAdaFormat, openInNewTab } from "@utils";
import { LoadingButtonProps } from "@atoms";
import { PendingTransaction } from "@/context/pendingTransaction";
import { VoterInfo } from "@/models";

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

  const cardProps: Partial<DashboardActionCardProps> = (() => {
    // transaction in progress
    if (
      !!pendingTransaction.registerAsSoleVoter ||
      !!pendingTransaction.retireAsSoleVoter
    ) {
      return {
        buttons: [
          {
            children: t("seeTransaction"),
            dataTestId: "see-transaction-button",
            onClick: () =>
              openInNewTab("https://adanordic.com/latest_transactions"),
          },
        ],
        state: "inProgress",
        ...(pendingTransaction.registerAsSoleVoter && {
          description: t("dashboard.soleVoter.registrationInProgress"),
          title: t("dashboard.soleVoter.registration"),
        }),
        ...(pendingTransaction.retireAsSoleVoter && {
          description: t("dashboard.soleVoter.retirementInProgress"),
          title: t("dashboard.soleVoter.retirement"),
        }),
      };
    }

    // learn more button
    const learnMoreButton: LoadingButtonProps = {
      children: t("learnMore"),
      dataTestId: "learn-more-button",
      onClick: () =>
        openInNewTab(
          "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
        ),
    };

    // currently registered
    if (voter?.isRegisteredAsSoleVoter) {
      return {
        buttons: [
          {
            children: t("dashboard.soleVoter.retire"),
            dataTestId: "retire-as-sole-voter-button",
            onClick: () => navigate(PATHS.retireAsSoleVoter),
          },
          learnMoreButton,
        ],
        description: <Trans i18nKey="dashboard.soleVoter.isRegisteredDescription" values={{ votingPower: ada }} />,
        state: "active",
        title: t("dashboard.soleVoter.youAreSoleVoterTitle"),
      };
    }

    // was registered
    if (voter?.wasRegisteredAsSoleVoter) {
      return {
        buttons: [
          {
            children: t("dashboard.soleVoter.reRegister"),
            dataTestId: "register-as-sole-voter-button",
            onClick: () => navigate(PATHS.registerAsSoleVoter),
          },
          learnMoreButton,
        ],
        description: <Trans i18nKey="dashboard.soleVoter.wasRegisteredDescription" values={{ votingPower: ada }} />,
        title: t("dashboard.soleVoter.wasSoleVoterTitle"),
      };
    }

    // not registered
    return {
      buttons: [
        {
          children: t("dashboard.soleVoter.register"),
          dataTestId: "register-as-sole-voter-button",
          onClick: () => navigate(PATHS.registerAsSoleVoter),
          variant: "contained",
        },
        learnMoreButton,
      ],
      description: <Trans i18nKey="dashboard.soleVoter.registerDescription" values={{ votingPower: ada }} />,
      title: t("dashboard.soleVoter.registerTitle"),
    };
  })();

  return (
    <DashboardActionCard imageURL={IMAGES.soleVoterImage} {...cardProps} />
  );
};
