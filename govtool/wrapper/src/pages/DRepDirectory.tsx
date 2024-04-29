import { Outlet } from "react-router-dom";
import { Box } from "@mui/material";

import { Background, PagePaddingBox, ContentBox } from "@atoms";
import { useTranslation } from "@hooks";
import { PageTitle } from "@molecules";
import { Footer, TopNav } from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const DRepDirectory = () => {
  const { t } = useTranslation();

  const isConnected = !checkIsWalletConnected();

  if (isConnected) {
    return (
      <PagePaddingBox
        sx={{ display: "flex", flex: 1, flexDirection: "column", py: 2 }}
      >
        <Outlet />
      </PagePaddingBox>
    );
  }

  return (
    <Background>
      <Box
        sx={{ minHeight: "100vh", display: "flex", flexDirection: "column" }}
      >
        <TopNav />

        <PageTitle title={t("dRepDirectory.title")} />

        <PagePaddingBox sx={{ display: "flex", flex: 1, py: 2 }}>
          <ContentBox
            sx={{ display: "flex", flex: 1, flexDirection: "column" }}
          >
            <Outlet />
          </ContentBox>
        </PagePaddingBox>
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
