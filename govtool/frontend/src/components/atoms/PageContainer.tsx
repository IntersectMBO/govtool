import { ElementType, ReactNode } from "react";
import { Box, SxProps, Theme } from "@mui/material";

type PageContainerProps = {
  children: ReactNode;
  sx?: SxProps<Theme>;
  /** "main" where the column is the page's content (not inside Dashboard). */
  component?: ElementType;
};

/** The redesign's page column: 1180px wide, centred, with side gutters. */
export const PageContainer = ({ children, sx, component = "div" }: PageContainerProps) => (
  <Box
    component={component}
    sx={{
      boxSizing: "border-box",
      maxWidth: 1180,
      mx: "auto",
      px: { xxs: 2, md: 3, xl: 0 },
      py: { xxs: 3, md: 5 },
      width: "100%",
      ...sx,
    }}
  >
    {children}
  </Box>
);
