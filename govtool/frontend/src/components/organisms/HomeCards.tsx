import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { IMAGES, PATHS } from "@consts";
import { useModal } from "@context";
import { ActionCard } from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";
import { useCallback } from "react";

export const HomeCards = () => {
  const navigate = useNavigate();
  const { openModal } = useModal();
  const { isMobile, screenWidth } = useScreenDimension();
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
    () => openInNewTab("https://www.google.com"),
    []
  );

  const navigateToGovActions = useCallback(
    () => navigate(PATHS.governance_actions),
    [navigate]
  );

  return (
    <Box
      columnGap={12}
      display="grid"
      gridTemplateColumns={screenWidth >= 1920 ? "1fr 1fr 1fr" : "1fr"}
      mb={6}
      mt={screenWidth < 1024 ? 6 : 14}
      px={
        screenWidth < 768
          ? 2
          : screenWidth < 1024
          ? 12
          : screenWidth < 1440
          ? 24
          : 34
      }
      rowGap={6}
    >
      <Box
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
        display="flex"
      >
        {/* DELEGATE CARD */}
        <ActionCard
          dataTestIdFirstButton="delegate-connect-wallet-button"
          dataTestIdSecondButton="delegate-learn-more-button"
          description={t("home.cards.delegateDescription")}
          firstButtonAction={openWalletModal}
          firstButtonLabel={t("home.cards.delegateFirstButtonLabel")}
          imageHeight={80}
          imageURL={IMAGES.govActionDelegateImage}
          imageWidth={115}
          secondButtonAction={onClickLearnMoreAboutDelegation}
          secondButtonLabel={t("learnMore")}
          title={t("home.cards.delegateTitle")}
        />
        {/* DELEGATE CARD END*/}
      </Box>
      <Box
        display="flex"
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
      >
        {/* REGISTER AS DREP CARD */}
        <ActionCard
          dataTestIdFirstButton="register-connect-wallet-button"
          dataTestIdSecondButton="register-learn-more-button"
          description={t("home.cards.registerAsDRepDescription")}
          firstButtonAction={openWalletModal}
          firstButtonLabel={t("home.cards.registerAsDRepFirstButtonLabel")}
          imageHeight={80}
          imageURL={IMAGES.govActionRegisterImage}
          imageWidth={70}
          secondButtonAction={onClickLearnMoreAboutDRepRegistration}
          secondButtonLabel={t("learnMore")}
          title={t("home.cards.registerAsDRepTitle")}
        />
        {/* REGISTER AS DREP CARD END */}
      </Box>
      <Box
        display="flex"
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
      >
        {/* REGISTER AS SOLE VOTER CARD */}
        <ActionCard
          dataTestIdFirstButton="register-as-sole-voter-button"
          dataTestIdSecondButton="lear-more-about-sole-voter-button"
          description={t("home.cards.registerAsSoleVoterDescription")}
          firstButtonAction={openWalletModal}
          firstButtonLabel={t("home.cards.registerAsSoleVoterFirstButtonLabel")}
          imageHeight={84}
          imageURL={IMAGES.soleVoterImage}
          imageWidth={84}
          secondButtonAction={onClickLearnMoreAboutSoleVoterRegistration}
          secondButtonLabel={t("learnMore")}
          title={t("home.cards.registerAsSoleVoterTitle")}
        />
        {/* REGISTER AS SOLE VOTER CARD END */}
      </Box>
      <Box
        display="flex"
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
      >
        {/* GOV ACTIONS CARD */}
        <ActionCard
          dataTestIdFirstButton="move-to-governance-actions-button"
          description={t("home.cards.governaneActionsDescription")}
          firstButtonAction={navigateToGovActions}
          firstButtonLabel={t("home.cards.governanceActionsFirstButtonLabel")}
          imageHeight={80}
          imageURL={IMAGES.govActionListImage}
          imageWidth={80}
          title={t("home.cards.governaneActionsTitle")}
        />
        {/* GOV ACTIONS CARD */}
      </Box>
    </Box>
  );
};
