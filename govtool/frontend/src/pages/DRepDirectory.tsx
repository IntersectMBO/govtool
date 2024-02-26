
import { useTranslation } from "@hooks";
import { MainLayout } from "@/components/organisms/MainLayout";
import { DRepDirectoryContent } from ".";

export const DRepDirectory = () => {
  const { t } = useTranslation();

  return (
    <MainLayout
      title={t('dRepDirectory.title')}
      isConnected={false}
      p={0}
    >
      <DRepDirectoryContent />
    </MainLayout>
  );
};
