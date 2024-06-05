import { Box } from "@mui/material";
import PDF from "@intersect.mbo/pdf-ui/cjs";
import "@intersect.mbo/pdf-ui/style";
import { useCardano } from "@/context";
import { TopNav } from "./TopNav";

export const PDFWrapper = () => {
  const walletAPI = useCardano();
  const isWalletConnected = walletAPI.isEnabled;

  return (
    <Box
      sx={{
        px: { xs: 2, sm: 5 },
        py: 3,
      }}
    >
      {!isWalletConnected && <TopNav />}
      <PDF walletAPI={{ ...walletAPI }} pathname={window.location.pathname} />
    </Box>
  );
};
