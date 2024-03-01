import { Box } from "@mui/material";
import { FC } from "react";

import { Button, Typography } from "@atoms";
import { theme } from "@/theme";
import { useScreenDimension } from "@hooks";

type ActionCardProps = {
  description?: string;
  firstButtonAction?: () => void;
  firstButtonLabel?: string;
  imageHeight?: number;
  imageURL?: string;
  imageWidth?: number;
  secondButtonAction?: () => void;
  secondButtonLabel?: string;
  title?: string;
  dataTestIdFirstButton?: string;
  dataTestIdSecondButton?: string;
};

export const ActionCard: FC<ActionCardProps> = ({ ...props }) => {
  const {
    dataTestIdFirstButton,
    dataTestIdSecondButton,
    description,
    firstButtonAction,
    firstButtonLabel,
    imageHeight = 80,
    imageURL,
    imageWidth = 80,
    secondButtonAction,
    secondButtonLabel,
    title,
  } = props;
  const { isMobile, screenWidth } = useScreenDimension();

  const {
    palette: { boxShadow2 },
  } = theme;

  return (
    <Box
      borderRadius={3}
      display="flex"
      flexDirection="column"
      flexGrow={1}
      maxWidth={798}
      p={4.25}
      sx={{ boxShadow: `5px 5px 15px 5px ${boxShadow2}` }}
      width="-webkit-fill-available"
    >
      <Box display="flex" flexDirection="column" flex={1}>
        {imageURL ? (
          <img
            src={imageURL}
            width={imageWidth}
            height={imageHeight}
            style={{
              alignSelf: screenWidth < 640 ? "center" : "start",
            }}
          />
        ) : null}
        {title ? (
          <Typography
            fontWeight={isMobile ? 600 : 500}
            sx={{
              mt: screenWidth < 640 ? 4 : 2.5,
              textAlign: screenWidth < 640 ? "center" : "left",
            }}
            variant={isMobile ? "title2" : "headline5"}
          >
            {title}
          </Typography>
        ) : null}
        {description ? (
          <Typography
            fontWeight={400}
            sx={{
              mb: 4.25,
              mt: 1.75,
              textAlign: screenWidth < 640 ? "center" : "left",
            }}
            variant={isMobile ? "body2" : "body1"}
          >
            {description}
          </Typography>
        ) : null}
      </Box>
      <Box display="flex" flexDirection={screenWidth < 640 ? "column" : "row"}>
        {firstButtonLabel ? (
          <Button
            data-testid={dataTestIdFirstButton}
            onClick={firstButtonAction}
            sx={{
              width: screenWidth < 640 ? "100%" : "auto",
            }}
          >
            {firstButtonLabel}
          </Button>
        ) : null}
        <Button
          data-testid={dataTestIdSecondButton}
          onClick={secondButtonAction}
          sx={{
            visibility: secondButtonLabel ? "visible" : "hidden",
            ml: screenWidth < 640 ? 0 : 2,
            mt: screenWidth < 640 ? 2 : 0,
            width: screenWidth < 640 ? "100%" : "auto",
            display: !secondButtonLabel && screenWidth < 768 ? "none" : "block",
          }}
          variant="outlined"
        >
          {secondButtonLabel}
        </Button>
      </Box>
    </Box>
  );
};
