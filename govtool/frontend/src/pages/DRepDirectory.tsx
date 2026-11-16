import { Outlet } from "react-router";
import { Box } from "@mui/material";

import { Background, PageContainer } from "@atoms";
import { Footer, TopNav } from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const DRepDirectory = () => {
  if (checkIsWalletConnected()) {
    return (
      <PageContainer sx={{ display: "flex", flex: 1, flexDirection: "column" }}>
        <Outlet />
      </PageContainer>
    );
  }

  return (
    <Background>
      <Box
        sx={{ minHeight: "100vh", display: "flex", flexDirection: "column" }}
      >
        <TopNav />
        <PageContainer component="main" sx={{ display: "flex", flex: 1, flexDirection: "column" }}>
          <Outlet />
        </PageContainer>
        <Footer />
      </Box>
    </Background>
  );
};
