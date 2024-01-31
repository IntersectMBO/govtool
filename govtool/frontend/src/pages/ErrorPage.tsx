import { useLocation, useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background, Button, Typography } from "@atoms";
import { IMAGES, PATHS } from "@consts";
import { useCardano } from "@context";
import { useScreenDimension } from "@hooks";

const IMAGE_SIZE = 748;

export const ErrorPage = ({
  errorCode = 404,
  errorDescription = "This page is not available",
  isButton = true,
}: {
  errorCode?: string | number;
  errorDescription?: string;
  isButton?: boolean;
}) => {
  const navigate = useNavigate();
  const { isEnabled } = useCardano();
  const { screenWidth } = useScreenDimension();
  const { state } = useLocation();

  return (
    <Background>
      <Box
        display="flex"
        flexDirection="column"
        justifyContent="center"
        minHeight="100vh"
        overflow="hidden"
        paddingX={screenWidth < 1024 ? 4 : 22.5}
        position="relative"
      >
        <Box>
          <Typography
            fontSize={57}
            fontWeight={700}
            lineHeight={"64px"}
            sx={{ whiteSpace: "nowrap" }}
          >
            Whoops!
          </Typography>
          <Typography sx={{ marginTop: 1 }} variant="headline3">
            {state && state.errorCode === 500
              ? "We have an internal server error."
              : errorDescription}
          </Typography>
          <Typography fontWeight={400} sx={{ marginY: 4.25 }} variant="title2">
            Error {state ? state.errorCode : errorCode}
          </Typography>
          {isButton && (
            <Button size="extraLarge" onClick={() => navigate(PATHS.home)}>
              {isEnabled ? "Back to dashboard" : "Back to homepage"}
            </Button>
          )}
        </Box>
        <Box
          flex={1}
          paddingY={3}
          position="absolute"
          right={screenWidth < 1024 ? -400 : -50}
          zIndex={-1}
        >
          <img
            height={IMAGE_SIZE}
            src={IMAGES.errorPageImage}
            width={IMAGE_SIZE}
          />
        </Box>
      </Box>
    </Background>
  );
};
