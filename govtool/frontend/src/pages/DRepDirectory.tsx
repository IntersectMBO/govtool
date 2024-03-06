import { useTranslation } from "@hooks";
import { checkIsWalletConnected } from "@/utils";
import { Background, PagePaddingBox, ContentBox } from "@/components/atoms";
import { DRepDirectoryContent, TopNav } from "@/components/organisms";
import { PageTitle } from "@/components/molecules";

export const DRepDirectory = () => {
  const { t } = useTranslation();

  const isConnected = !checkIsWalletConnected();

  if (isConnected) {
    return (
      <PagePaddingBox py={2}>
        <DRepDirectoryContent isConnected />
      </PagePaddingBox>
    );
  }

  return (
    <Background>
      <TopNav />

      <PageTitle title={t("dRepDirectory.title")} />

      <PagePaddingBox py={2}>
        <ContentBox>
          <DRepDirectoryContent />
        </ContentBox>
      </PagePaddingBox>
    </Background>
  );
};
