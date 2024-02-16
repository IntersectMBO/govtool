import { Box } from "@mui/material";
import { SpacerProps } from ".";

export const Spacer = ({ x, y }: SpacerProps) => {
  return <Box pt={y} pr={x} />;
};
