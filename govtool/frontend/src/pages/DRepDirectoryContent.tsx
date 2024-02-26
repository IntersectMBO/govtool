
import { useTranslation } from "@hooks";
import { Box } from "@mui/material";
import { FC } from "react";

interface Props {
  isConnected?: boolean;
}

export const DRepDirectoryContent: FC<Props> = () => {
  const { t } = useTranslation();

  return (
    <Box flex={1} px={5} py={1.5}>
      {t('dRepDirectory.title')}
    </Box>
  );
};
