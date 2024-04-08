import { useTranslation } from "@hooks";
import { Outlet } from "react-router-dom";
import { checkIsWalletConnected } from "@/utils";
import { Background, PagePaddingBox, ContentBox } from "@/components/atoms";
import { Footer, TopNav } from "@/components/organisms";
import { PageTitle } from "@/components/molecules";

export const DRepDirectory = () => {
  const { t } = useTranslation();

  const isConnected = !checkIsWalletConnected();

  if (isConnected) {
    return (
      <PagePaddingBox py={2}>
        <Outlet />
      </PagePaddingBox>
    );
  }

  return (
    <Background>
      <TopNav />

      <PageTitle title={t("dRepDirectory.title")} />

      <PagePaddingBox py={2}>
        <ContentBox>
          <Outlet />
        </ContentBox>
      </PagePaddingBox>
      <Footer />
    </Background>
  );
};
