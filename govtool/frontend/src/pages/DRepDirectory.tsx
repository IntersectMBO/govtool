import { Outlet } from "react-router";
import { Box } from "@mui/material";

import { Background, PageContainer } from "@atoms";
import { Footer, TopNav } from "@organisms";

type DRepDirectoryProps = {
  isConnected?: boolean;
};

// The dashboard supplies its own chrome; the public page adds it here.
export const DRepDirectory = ({ isConnected = false }: DRepDirectoryProps) => {
  if (isConnected) {
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
