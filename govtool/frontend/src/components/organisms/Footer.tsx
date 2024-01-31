import { Box, Link } from "@mui/material";

import { Typography } from "@atoms";
import { useScreenDimension } from "@hooks";
import { openInNewTab } from "@utils";

export const Footer = () => {
  const { isMobile, pagePadding } = useScreenDimension();

  return (
    <Box
      display="flex"
      flexDirection={isMobile ? "column" : "row"}
      justifyContent="space-between"
      px={pagePadding}
      py={4}
    >
      <Box flex={1}>
        <Typography fontWeight={500} variant="caption">
          © 2023 Voltaire Gov Tool
        </Typography>
      </Box>
      <Box display="flex" flexDirection="row" mt={isMobile ? 1.5 : 0}>
        <Link
          data-testid="privacy-policy-link"
          onClick={() =>
            openInNewTab("https://docs.sanchogov.tools/legal/privacy-policy")
          }
          sx={[{ textDecoration: "none" }]}
          mr={6}
        >
          <Typography
            fontWeight={isMobile ? 300 : 500}
            sx={{ "&:hover": { color: "primaryBlue", cursor: "pointer" } }}
            variant="caption"
          >
            Privacy policy
          </Typography>
        </Link>
      </Box>
    </Box>
  );
};
