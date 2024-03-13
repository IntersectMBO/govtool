import { useCallback } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { IMAGES, PATHS } from "@consts";
import { useModal } from "@context";
import { ActionCard } from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

export const HomeCards = () => {
  const navigate = useNavigate();
  const { openModal } = useModal();
  const { screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  const openWalletModal = useCallback(() => {
    openModal({ type: "chooseWallet" });
  }, [openModal]);

  const onClickLearnMoreAboutDelegation = useCallback(
    () =>
      openInNewTab(
        "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power"
      ),
    []
  );

  const onClickLearnMoreAboutDRepRegistration = useCallback(
    () =>
      openInNewTab(
        "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
      ),
    []
  );

  const onClickLearnMoreAboutSoleVoterRegistration = useCallback(
    // TODO: Update the link
    () => openInNewTab("https://docs.sanchogov.tools/"),
    []
  );

  const onClickLearnMoreAboutProposingGovAction = useCallback(
    // TODO: Update the link
    () => openInNewTab("https://docs.sanchogov.tools/"),
    []
  );

  const navigateToGovActions = useCallback(
    () => navigate(PATHS.governanceActions),
    [navigate]
  );

  return (
    <Box
      columnGap={5}
      display="grid"
      gridTemplateColumns={screenWidth >= 1920 ? "1fr 1fr" : "1fr"}
      justifyItems="center"
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
      rowGap={5}
    >
      {/* DELEGATE CARD */}
      <ActionCard
        dataTestIdFirstButton="delegate-connect-wallet-button"
        dataTestIdSecondButton="delegate-learn-more-button"
        description={t("home.cards.delegate.description")}
        firstButtonAction={openWalletModal}
        firstButtonLabel={t("home.cards.delegate.firstButtonLabel")}
        imageHeight={80}
        imageURL={IMAGES.govActionDelegateImage}
        imageWidth={115}
        secondButtonAction={onClickLearnMoreAboutDelegation}
        secondButtonLabel={t("learnMore")}
        title={t("home.cards.delegate.title")}
      />
      {/* DELEGATE CARD END*/}
      {/* REGISTER AS DREP CARD */}
      <ActionCard
        dataTestIdFirstButton="register-connect-wallet-button"
        dataTestIdSecondButton="register-learn-more-button"
        description={t("home.cards.registerAsDRep.description")}
        firstButtonAction={openWalletModal}
        firstButtonLabel={t("home.cards.registerAsDRep.firstButtonLabel")}
        imageHeight={80}
        imageURL={IMAGES.govActionRegisterImage}
        imageWidth={70}
        secondButtonAction={onClickLearnMoreAboutDRepRegistration}
        secondButtonLabel={t("learnMore")}
        title={t("home.cards.registerAsDRep.title")}
      />
      {/* REGISTER AS DREP CARD END */}
      {/* REGISTER AS SOLE VOTER CARD */}
      <ActionCard
        dataTestIdFirstButton="register-as-sole-voter-button"
        dataTestIdSecondButton="lear-more-about-sole-voter-button"
        description={t("home.cards.registerAsSoleVoter.description")}
        firstButtonAction={openWalletModal}
        firstButtonLabel={t("home.cards.registerAsSoleVoter.firstButtonLabel")}
        imageHeight={84}
        imageURL={IMAGES.soleVoterImage}
        imageWidth={84}
        secondButtonAction={onClickLearnMoreAboutSoleVoterRegistration}
        secondButtonLabel={t("learnMore")}
        title={t("home.cards.registerAsSoleVoter.title")}
      />
      {/* REGISTER AS SOLE VOTER CARD END */}
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
      {/* GOV ACTIONS CARD  END*/}
      {/* PROPOSE GOV ACTION CARD  */}
      <ActionCard
        dataTestIdFirstButton="propose-a-governance-action-button"
        description={t("home.cards.proposeAGovernanceAction.description")}
        firstButtonAction={openWalletModal}
        firstButtonLabel={t(
          "home.cards.proposeAGovernanceAction.firstButtonLabel"
        )}
        imageHeight={84}
        imageURL={IMAGES.proposeGovActionImage}
        imageWidth={84}
        secondButtonAction={onClickLearnMoreAboutProposingGovAction}
        secondButtonLabel={t("learnMore")}
        title={t("home.cards.proposeAGovernanceAction.title")}
      />
      {/* PROPOSE GOV ACTION CARD  END*/}
    </Box>
  );
};
