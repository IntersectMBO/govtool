import { useScreenDimension } from "@/hooks";
import { Box } from "@mui/material";

interface Props {
  children: React.ReactNode;
}

export function ModalContents({ children }: Props) {
  const { isMobile } = useScreenDimension();

  return (
    <Box
      display="flex"
      flexDirection="column"
      alignItems="center"
      px={isMobile ? 0 : 3}
    >
      {children}
    </Box>
  );
}
