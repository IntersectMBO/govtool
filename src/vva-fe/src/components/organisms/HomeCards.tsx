import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { IMAGES, PATHS } from "@consts";
import { useModal } from "@context";
import { ActionCard } from "@molecules";
import { useScreenDimension } from "@hooks";
import { openInNewTab } from "@utils";

export const HomeCards = () => {
  const navigate = useNavigate();
  const { openModal } = useModal();
  const { isMobile, screenWidth } = useScreenDimension();

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
          description="If you want to delegate to a DRep or select a default option."
          firstButtonAction={() => openModal({ type: "chooseWallet" })}
          firstButtonLabel="Connect to delegate"
          imageHeight={80}
          imageURL={IMAGES.govActionDelegateImage}
          imageWidth={115}
          secondButtonAction={() =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/ways-to-use-your-voting-power"
            )
          }
          secondButtonLabel="Learn more"
          title="Use your Voting Power"
        />
      </Box>
      <Box
        display="flex"
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
      >
        <ActionCard
          dataTestIdFirstButton="register-connect-wallet-button"
          dataTestIdSecondButton="register-learn-more-button"
          description="If you want to directly participate in voting and have other ada holders delegate their voting power to you."
          firstButtonAction={() => openModal({ type: "chooseWallet" })}
          firstButtonLabel="Connect to register"
          imageHeight={80}
          imageURL={IMAGES.govActionRegisterImage}
          imageWidth={70}
          secondButtonAction={() =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/what-does-it-mean-to-register-as-a-drep"
            )
          }
          secondButtonLabel="Learn more"
          title="Register as a DRep"
        />
      </Box>
      <Box
        display="flex"
        width={!isMobile && screenWidth < 1920 ? "90%" : "auto"}
      >
        <ActionCard
          dataTestIdFirstButton="move-to-governance-actions-button"
          description="Review governance actions submitted on-chain."
          firstButtonAction={() => navigate(PATHS.governance_actions)}
          firstButtonLabel="View governance actions"
          imageHeight={80}
          imageURL={IMAGES.govActionListImage}
          imageWidth={80}
          title="Governance Actions"
        />
      </Box>
    </Box>
  );
};
