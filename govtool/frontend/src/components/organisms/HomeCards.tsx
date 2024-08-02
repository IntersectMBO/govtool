import { useCallback } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { IMAGES, PATHS, PDF_PATHS } from "@consts";
import { useFeatureFlag, useModal } from "@context";
import { ActionCard } from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

export const HomeCards = () => {
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const navigate = useNavigate();
  const { openModal } = useModal();
  const { screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  const openWalletModal = useCallback(
    (pathToNavigate: string) => {
      openModal({
        type: "chooseWallet",
        state: {
          pathToNavigate,
        },
      });
    },
    [openModal],
  );

  const onClickLearnMoreAboutDelegation = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/how-to-use-the-govtool/using-govtool/delegating",
    );

  const onClickLearnMoreAboutDRepRegistration = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/how-to-use-the-govtool/using-govtool/dreps",
    );

  const onClickLearnMoreAboutDirectVoterRegistration = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/how-to-use-the-govtool/using-govtool/direct-voting",
    );

  const onClickLearnMoreAboutProposingGovAction = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/how-to-use-the-govtool/using-govtool/governance-actions/propose-a-governance-action",
    );

  const navigateToGovActions = useCallback(
    () => navigate(PATHS.governanceActions),
    [navigate],
  );

  const navigateToDRepDirecotry = useCallback(
    () => navigate(PATHS.dRepDirectory),
    [navigate],
  );

  const navigateToProposalDiscussionPillar = useCallback(
    () => navigate(PDF_PATHS.proposalDiscussion),
    [navigate],
  );

  return (
    <Box
      columnGap={4.625}
      display="grid"
      gridTemplateColumns={
        screenWidth < 2560
          ? "repeat(1, minmax(300px, 866px))"
          : "repeat(2, minmax(300px, 866px))"
      }
      justifyContent="center"
      mb={screenWidth < 640 ? 10 : 17}
      mt={screenWidth < 640 ? 10 : 14.5}
      px={
        screenWidth < 640
          ? 2
          : screenWidth < 1024
          ? 5
          : screenWidth < 1440
          ? 10
          : 34
      }
      rowGap={4.625}
    >
      {/* DELEGATE CARD */}
      <ActionCard
        dataTestIdFirstButton="view-drep-directory-button"
        dataTestIdSecondButton="delegate-learn-more-button"
        description={t("home.cards.delegate.description")}
        firstButtonAction={navigateToDRepDirecotry}
        firstButtonLabel={t("home.cards.delegate.firstButtonLabel")}
        imageHeight={80}
        imageURL={IMAGES.govActionDelegateImage}
        imageWidth={115}
        secondButtonAction={onClickLearnMoreAboutDelegation}
        secondButtonLabel={t("learnMore")}
        title={t("home.cards.delegate.title")}
      />
      {/* DELEGATE CARD END */}
      {/* REGISTER AS DREP CARD */}
      <ActionCard
        dataTestIdFirstButton="register-connect-wallet-button"
        dataTestIdSecondButton="register-learn-more-button"
        description={t("home.cards.registerAsDRep.description")}
        firstButtonAction={() => openWalletModal(PATHS.registerAsdRep)}
        firstButtonLabel={t("home.cards.registerAsDRep.firstButtonLabel")}
        imageHeight={80}
        imageURL={IMAGES.govActionRegisterImage}
        imageWidth={70}
        secondButtonAction={onClickLearnMoreAboutDRepRegistration}
        secondButtonLabel={t("learnMore")}
        title={t("home.cards.registerAsDRep.title")}
      />
      {/* REGISTER AS DREP CARD END */}
      {/* REGISTER AS DIRECT VOTER CARD */}
      <ActionCard
        dataTestIdFirstButton="register-as-sole-voter-button"
        dataTestIdSecondButton="lear-more-about-sole-voter-button"
        description={t("home.cards.registerAsDirectVoter.description")}
        firstButtonAction={() => openWalletModal(PATHS.registerAsDirectVoter)}
        firstButtonLabel={t(
          "home.cards.registerAsDirectVoter.firstButtonLabel",
        )}
        imageHeight={84}
        imageURL={IMAGES.directVoterImage}
        imageWidth={84}
        secondButtonAction={onClickLearnMoreAboutDirectVoterRegistration}
        secondButtonLabel={t("learnMore")}
        title={t("home.cards.registerAsDirectVoter.title")}
      />
      {/* REGISTER AS DIRECT VOTER CARD END */}
      {/* GOV ACTIONS CARD */}
      <ActionCard
        dataTestIdFirstButton="move-to-governance-actions-button"
        description={t("home.cards.governanceActions.description")}
        firstButtonAction={navigateToGovActions}
        firstButtonLabel={t("home.cards.governanceActions.firstButtonLabel")}
        imageHeight={84}
        imageURL={IMAGES.govActionListImage}
        imageWidth={84}
        title={t("home.cards.governanceActions.title")}
      />
      {/* GOV ACTIONS CARD  END */}
      {/* PROPOSE GOV ACTION CARD  */}
      <ActionCard
        dataTestIdFirstButton="propose-a-governance-action-button"
        description={t("home.cards.proposeAGovernanceAction.description")}
        firstButtonAction={() => openWalletModal(PATHS.createGovernanceAction)}
        firstButtonLabel={t(
          "home.cards.proposeAGovernanceAction.firstButtonLabel",
        )}
        imageHeight={84}
        imageURL={IMAGES.proposeGovActionImage}
        imageWidth={84}
        secondButtonAction={
          isProposalDiscussionForumEnabled
            ? navigateToProposalDiscussionPillar
            : onClickLearnMoreAboutProposingGovAction
        }
        secondButtonLabel={t(
          isProposalDiscussionForumEnabled
            ? "home.cards.proposeAGovernanceAction.secondButtonLabel"
            : "learnMore",
        )}
        title={t("home.cards.proposeAGovernanceAction.title")}
      />
      {/* PROPOSE GOV ACTION CARD  END */}
    </Box>
  );
};
