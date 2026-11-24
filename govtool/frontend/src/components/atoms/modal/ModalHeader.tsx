import Typography from "@mui/material/Typography";
import type { SxProps } from "@mui/system";

interface Props {
  children: React.ReactNode;
  sx?: SxProps;
}

export const ModalHeader = ({ children, sx }: Props) => (
  <Typography
    color="#101828"
    component="h2"
    fontSize="22px"
    fontWeight="600"
    letterSpacing="-0.3px"
    lineHeight="30px"
    marginBottom="8px"
    textAlign="center"
    sx={sx}
  >
    {children}
  </Typography>
);
