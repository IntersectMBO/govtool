import { Box } from "@mui/material";

import { useScreenDimension } from "@/hooks";
import { Background } from "@atoms";

import { TopNav, ChooseStakeKeyPanel, Footer } from "@organisms";

export const ChooseStakeKey = () => {
  const { isMobile } = useScreenDimension();

  return (
    <Background>
      <Box display="flex" flexDirection={"column"} minHeight="100vh">
        <TopNav isConnectButton={false} />
        <Box display={"flex"} flex={1} justifyContent={"center"}>
          <ChooseStakeKeyPanel />
        </Box>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
