import { Box } from '@mui/material';

import { Background } from '@atoms';

import { TopNav, ChooseStakeKeyPanel, Footer } from '@organisms';
import { useScreenDimension } from '@/hooks';

export const ChooseStakeKey = () => {
  const { isMobile } = useScreenDimension();

  return (
    <Background>
      <Box display="flex" flexDirection="column" minHeight="100vh">
        <TopNav isConnectButton={false} />
        <Box display="flex" flex={1} justifyContent="center">
          <ChooseStakeKeyPanel />
        </Box>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
