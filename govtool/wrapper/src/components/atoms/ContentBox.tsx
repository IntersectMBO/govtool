import { Box, BoxProps } from "@mui/material";
import { FC } from "react";

export const ContentBox: FC<BoxProps> = ({ children, ...props }) => (
  <Box maxWidth={1290} mx="auto" {...props}>
    {children}
  </Box>
);
