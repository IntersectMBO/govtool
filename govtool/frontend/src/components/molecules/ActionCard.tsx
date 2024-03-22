import { Box } from "@mui/material";
import { FC } from "react";

import { Button, Typography } from "@atoms";
import { useScreenDimension } from "@hooks";
import { theme } from "@/theme";

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
  const { screenWidth } = useScreenDimension();

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
            alt="action-card"
            height={imageHeight}
            src={imageURL}
            width={imageWidth}
          />
        ) : null}
        {title ? (
          <Typography
            sx={{
              mt: 2.5,
            }}
            variant="headline5"
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
            }}
            variant="body1"
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
            display: !secondButtonLabel && screenWidth < 768 ? "none" : "block",
            ml: screenWidth < 640 ? 0 : 2,
            mt: screenWidth < 640 ? 2 : 0,
            visibility: secondButtonLabel ? "visible" : "hidden",
            width: screenWidth < 640 ? "100%" : "auto",
          }}
          variant="outlined"
        >
          {secondButtonLabel}
        </Button>
      </Box>
    </Box>
  );
};
