import { Box } from "@mui/material";

import { Background } from "@atoms";

import { TopNav, ChooseStakeKeyPanel, Footer } from "@organisms";

export const ChooseStakeKey = () => (
  <Background>
    <Box display="flex" flexDirection="column" minHeight="100vh">
      <TopNav isConnectButton={false} />
      <Box display="flex" flex={1} justifyContent="center">
        <ChooseStakeKeyPanel />
      </Box>
      {/* FIXME: Footer should be on top of the layout.
          Should not be rerendered across the pages */}
      <Footer />
    </Box>
  </Background>
);
