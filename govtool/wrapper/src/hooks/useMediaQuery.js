import { useTheme, useMediaQuery as useMediaQueryMUI } from "@mui/material";

export const useMediaQuery = () => {
  const theme = useTheme();
  const desktop = useMediaQueryMUI(theme.breakpoints.up("lg"));
  const tablet = useMediaQueryMUI(theme.breakpoints.up("md"));
  const mobile = useMediaQueryMUI(theme.breakpoints.up("xxs"));

  return { desktop, tablet, mobile };
};
