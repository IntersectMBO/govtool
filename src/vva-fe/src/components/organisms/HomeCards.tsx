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
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

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
        <ActionCard
          dataTestIdFirstButton="delegate-connect-wallet-button"
          dataTestIdSecondButton="delegate-learn-more-button"
          description={t("dashboard.delegation.description")}
          firstButtonAction={() => openModal({ type: "chooseWallet" })}
          firstButtonLabel={t("dashboard.delegation.connectToDelegate")}
          imageHeight={80}
          imageURL={IMAGES.govActionDelegateImage}
          imageWidth={115}
          secondButtonAction={() =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power"
            )
          }
          secondButtonLabel={t("learnMore")}
          title={t("dashboard.delegation.useYourVotingPower")}
        />
      </Box>
      <Box
        display="flex"
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
      >
        <ActionCard
          dataTestIdFirstButton="register-connect-wallet-button"
          dataTestIdSecondButton="register-learn-more-button"
          description={t("dashboard.registration.description")}
          firstButtonAction={() => openModal({ type: "chooseWallet" })}
          firstButtonLabel={t("dashboard.registration.connectToRegister")}
          imageHeight={80}
          imageURL={IMAGES.govActionRegisterImage}
          imageWidth={70}
          secondButtonAction={() =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
            )
          }
          secondButtonLabel={t("learnMore")}
          title={t("dashboard.registration.registerAsDRep")}
        />
      </Box>
      <Box
        display="flex"
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
      >
        <ActionCard
          dataTestIdFirstButton="move-to-governance-actions-button"
          description={t("dashboard.govActions.description")}
          firstButtonAction={() => navigate(PATHS.governance_actions)}
          firstButtonLabel={t("dashboard.govActions.view")}
          imageHeight={80}
          imageURL={IMAGES.govActionListImage}
          imageWidth={80}
          title={t("dashboard.govActions.title")}
        />
      </Box>
    </Box>
  );
};
