import { Box, BoxProps } from "@mui/material";
import { FC } from "react";

export const PagePaddingBox: FC<BoxProps> = ({ children, ...props }) => (
  <Box px={{ xxs: 2, md: 5 }} {...props}>
    {children}
  </Box>
);
