import { Box } from "@mui/material";
import PDF from "@intersect.mbo/pdf-ui/cjs";
import "@intersect.mbo/pdf-ui/style";
import { useCardano, useGovernanceActions } from "@/context";

export const PDFWrapper = () => {
  const walletAPI = useCardano();
  const { createGovernanceActionJsonLD, createHash } = useGovernanceActions();

  return (
    <Box
      sx={{
        px: { xs: 2, sm: 5 },
        py: 3,
      }}
    >
      <PDF
        walletAPI={{ ...walletAPI, createGovernanceActionJsonLD, createHash }}
        pathname={window.location.pathname}
      />
    </Box>
  );
};
