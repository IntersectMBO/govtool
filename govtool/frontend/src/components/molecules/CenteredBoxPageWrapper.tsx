import { FC, PropsWithChildren } from "react";
import { Box, Link } from "@mui/material";

import { Background, Typography } from "@atoms";
import { ICONS } from "@consts";
import { DashboardTopNav } from "@organisms";
import { useScreenDimension } from "@hooks";
import { useNavigate } from "react-router-dom";
import { theme } from "@/theme";

interface Props {
  pageTitle: string;
  backButtonText: string;
  backButtonPath: string;
  isVotingPowerHidden?: boolean;
}
export const CenteredBoxPageWrapper: FC<PropsWithChildren<Props>> = ({
  pageTitle,
  backButtonText,
  backButtonPath,
  isVotingPowerHidden,
  children,
}) => {
  const { isMobile, screenWidth, pagePadding } = useScreenDimension();
  const navigate = useNavigate();
  const {
    palette: { boxShadow2 },
  } = theme;

  return (
    <Background isReverted>
      <Box display={"flex"} minHeight={"100vh"} flexDirection="column">
        <DashboardTopNav
          imageSRC={ICONS.appLogoIcon}
          imageWidth={isMobile ? undefined : 42}
          imageHeight={isMobile ? 24 : 35}
          title={pageTitle}
          isVotingPowerHidden={isVotingPowerHidden}
        />
        <Box
          display={"flex"}
          justifyContent={"center"}
          flexDirection={"column"}
          mt={isMobile ? 0 : 7}
          height={isMobile ? "100%" : "auto"}
          sx={{ marginTop: "97px" }}
        >
          {isMobile && (
            <Box borderBottom={1} borderColor={"#fff"}>
              <Typography
                variant="body2"
                sx={{
                  ml: 2,
                  my: "26px",
                  fontSize: "24px",
                  fontWeight: 400,
                }}
              >
                {pageTitle}
              </Typography>
            </Box>
          )}
          <Link
            data-testid={"back-button"}
            sx={{
              cursor: "pointer",
              display: "flex",
              textDecoration: "none",
              my: 3,
              marginLeft: isMobile ? 2 : "40px",
            }}
            onClick={() => navigate(backButtonPath)}
          >
            <img
              src={ICONS.arrowLeftThinIcon}
              alt="arrow"
              style={{ marginRight: "4px" }}
            />
            <Typography
              variant="body2"
              color="primary"
              sx={{
                fontWeight: 400,
                paddingTop: "1px",
              }}
            >
              {backButtonText}
            </Typography>
          </Link>
          <Box display={"flex"} justifyContent={"center"}>
            <Box
              width={screenWidth < 768 ? "auto" : "52vw"}
              boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
              px={pagePadding}
              py={isMobile ? 3 : 8}
              borderRadius={"20px"}
              height="auto"
            >
              <Box display="flex" flexDirection="column">
                {children}
              </Box>
            </Box>
          </Box>
        </Box>
      </Box>
    </Background>
  );
};
