import { Box } from "@mui/material";
import { ComponentProps, FC, PropsWithChildren } from "react";

import { ICONS } from "@consts";
import { Background, Typography } from "@atoms";
import { useScreenDimension } from "@hooks";
import {
  DashboardTopNav,
  DashboardTopNavProps,
  Drawer,
  Footer,
} from "@organisms";
import { NewTopNav } from "./NewTopNav";
import { theme } from "@/theme";

export type DashboardLayoutProps = PropsWithChildren &
  Pick<DashboardTopNavProps, "title"> &
  ComponentProps<typeof Box> & {
    isConnected: boolean;
    hideDrawer?: boolean;
  };

export const MainLayout: FC<DashboardLayoutProps> = ({
  children,
  sx,
  title,
  isConnected,
  hideDrawer,
  ...boxProps
}) => {
  const { isMobile } = useScreenDimension();

  const showDrawer = !!isConnected && !isMobile && !hideDrawer;

  return (
    <Background opacity={0.7}>
      <Box display="flex" flexDirection="row" position="relative">
        {showDrawer && <Drawer />}

        <Box
          flex={1}
          height="100vh"
          overflow="auto"
          display="flex"
          flexDirection="column"
        >
          {isConnected ? (
            <DashboardTopNav
              title={title}
              imageSRC={isMobile ? ICONS.appLogoIcon : undefined}
              imageHeight={24}
              isDrawer={!hideDrawer}
              sx={{ flexGrow: 0, position: "sticky", top: 0 }}
            />
          ) : (
            <>
              <NewTopNav />
              {title && (
                <Box
                  borderBottom={`1px solid ${theme.palette.neutralWhite}`}
                  px={{ xxs: 2, md: 5 }}
                  py={3}
                >
                  <Typography variant="headline5">{title}</Typography>
                </Box>
              )}
            </>
          )}
          <Box flex={1} px={5} py={1.5} {...boxProps}>
            {children}
          </Box>

          <Footer />
        </Box>
      </Box>
    </Background>
  );
};
