import { useEffect, useMemo, useState } from "react";
import { generatePath, Link } from "react-router-dom";
import { Box } from "@mui/material";
import { KeenSliderOptions } from "keen-slider";
import "keen-slider/keen-slider.min.css";

import { useCardano } from "@context";
import { useScreenDimension, useTranslation, useSlider } from "@hooks";
import { Button, Typography } from "@atoms";
import { SliderArrows } from "@molecules";
import { PATHS } from "@consts";
import { theme } from "@/theme";

type SliderProps = {
  title: string;
  navigateKey: string;
  data: React.ReactNode;
  isShowAll?: boolean;
  dataLength?: number;
  notSlicedDataLength?: number;
  onDashboard?: boolean;
  searchPhrase?: string;
  sorting?: string;
  filters?: string[];
};

export const Slider = ({
  data,
  title,
  navigateKey,
  isShowAll = true,
  dataLength = 0,
  notSlicedDataLength = 0,
  onDashboard = false,
  filters,
  searchPhrase,
  sorting,
}: SliderProps) => {
  const [isSliderInitialized, setIsSliderInitialized] = useState(false);

  const { isMobile, screenWidth } = useScreenDimension();
  const { pendingTransaction } = useCardano();
  const { t } = useTranslation();

  const {
    palette: { primaryBlue, arcticWhite, lightBlue },
  } = theme;

  const DEFAULT_SLIDER_CONFIG = {
    mode: "free",
    initial: 0,
    slides: {
      perView: "auto",
      spacing: 20,
    },
  } as KeenSliderOptions;

  const isShowArrows = useMemo(
    () =>
      // Arrows are to be show only on desktop view.
      // 268 - side menu width; 40 - distance needed from the left on
      // disconnected wallet (no side menu); 350 - gov action card width;
      // other values are for paddings and margins
      screenWidth <
      (onDashboard ? 268 : 40) + 28 + dataLength * 350 + 20 * dataLength - 5,
    [screenWidth, dataLength],
  );

  const { sliderRef, instanceRef, currentSlide, itemsPerView } = useSlider({
    config: DEFAULT_SLIDER_CONFIG,
  });

  const refresh = () => {
    instanceRef.current?.update(instanceRef.current?.options);
    instanceRef.current?.track.to(0);
    instanceRef.current?.moveToIdx(0);
  };

  useEffect(() => {
    if (instanceRef.current) {
      setIsSliderInitialized(true);
    }
  }, [instanceRef.current]);

  useEffect(() => {
    refresh();
  }, [
    filters,
    sorting,
    searchPhrase,
    pendingTransaction?.vote?.resourceId,
    data,
  ]);

  return (
    <Box>
      <Box
        sx={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          gap: "10px",
          mb: 3.5,
          height: "46px",
        }}
      >
        <Box
          sx={{
            display: "flex",
            justifyContent: "space-between",
            alignItems: "center",
            gap: 2,
          }}
        >
          <Typography variant="title2">{title}</Typography>
          {(notSlicedDataLength > 6 || (isMobile && isShowAll)) && (
            <Button
              component={Link}
              to={`${generatePath(
                  onDashboard
                      ? PATHS.dashboardGovernanceActionsCategory
                      : PATHS.governanceActionsCategory,
                  {
                    category: navigateKey,
                  },
              )}`}
              variant="contained"
              size="medium"
              sx={{
                border: `1px solid ${lightBlue}`,
                backgroundColor: arcticWhite,
                boxShadow: "none",
                color: primaryBlue,
                minWidth: 93,
                "&:hover": { backgroundColor: arcticWhite },
              }}
            >
              {t("slider.showAll")}
            </Button>
          )}
        </Box>
        {isSliderInitialized && isShowArrows && dataLength > 1 && !isMobile && (
          <SliderArrows
            currentSlide={currentSlide}
            instanceRef={instanceRef}
            itemsPerView={itemsPerView}
          />
        )}
      </Box>
      <div
        ref={sliderRef}
        className="keen-slider"
        style={{ width: "100%", overflow: "visible" }}
      >
        {data}
      </div>
    </Box>
  );
};
