import { ReactNode } from "react";
import { Box } from "@mui/material";

import { Typography } from "@/components/atoms";
import { ds } from "@/consts/designTokens";

type PageTitleProps = {
  title: string;
  description?: string;
  action?: ReactNode;
};

export const PageTitle = ({ title, description, action }: PageTitleProps) => (
  <Box
    sx={{
      alignItems: "center",
      display: "flex",
      flexWrap: "wrap",
      gap: 2,
      justifyContent: "space-between",
    }}
  >
    <Box>
      <Typography color={ds.gray["900"]} component="h1" variant="paragraphXL">
        {title}
      </Typography>
      {description && (
        <Typography
          color={ds.gray["500"]}
          sx={{ maxWidth: 467, mt: 1 }}
          variant="paragraphSM"
        >
          {description}
        </Typography>
      )}
    </Box>
    {action}
  </Box>
);
