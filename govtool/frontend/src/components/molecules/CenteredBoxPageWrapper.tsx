import { FC, PropsWithChildren } from "react";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { DashboardTopNav, Footer } from "@organisms";
import { useScreenDimension, useTranslation } from "@hooks";
import { useNavigate } from "react-router-dom";
import { theme } from "@/theme";
import { LinkWithIcon } from "./LinkWithIcon";

interface Props {
  pageTitle: string;
  onClickBackToDashboard?: () => void;
  showVotingPower?: boolean;
  hideBox?: boolean;
}
export const CenteredBoxPageWrapper: FC<PropsWithChildren<Props>> = ({
  pageTitle,
  onClickBackToDashboard,
  showVotingPower,
  hideBox,
  children,
}) => {
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();
  const {
    palette: { boxShadow2 },
  } = theme;

  return (
    <Background isReverted>
      <Box display="flex" minHeight="100vh" flexDirection="column">
        <DashboardTopNav title={pageTitle} hideVotingPower={!showVotingPower} />
        <Box
          flex={1}
          px={isMobile ? 2 : 5}
          py={isMobile ? 3 : 1.5}
          display="flex"
          flexDirection="column"
          gap={isMobile ? 0 : 1.5}
        >
          <LinkWithIcon
            label={t("backToDashboard")}
            onClick={
              onClickBackToDashboard ?? (() => navigate(PATHS.dashboard))
            }
          />
          {hideBox ? (
            <Box flex={1}>{children}</Box>
          ) : (
            <Box
              alignSelf="center"
              borderRadius="20px"
              boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
              mb={isMobile ? 2 : 1.5}
              px={isMobile ? 2 : 18.75}
              py={isMobile ? 6 : 8}
              height="auto"
              maxWidth={isMobile ? "none" : 600}
              display="flex"
              flexDirection="column"
            >
              {children}
            </Box>
          )}
        </Box>
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
