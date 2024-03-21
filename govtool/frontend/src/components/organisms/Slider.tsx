import { useCallback, useEffect, useMemo } from "react";
import { Box, Link, Typography } from "@mui/material";
import { KeenSliderOptions } from "keen-slider";
import "keen-slider/keen-slider.min.css";

import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import { useScreenDimension, useSlider, useTranslation } from "@hooks";
import { generatePath, useNavigate } from "react-router-dom";
import styles from "./slider.module.css";

const SLIDER_MAX_LENGTH = 1000;

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
  const { isMobile, screenWidth, pagePadding } = useScreenDimension();
  const navigate = useNavigate();
  const { voteTransaction } = useCardano();
  const { t } = useTranslation();

  const DEFAULT_SLIDER_CONFIG = {
    mode: "free",
    initial: 0,
    slides: {
      perView: "auto",
      spacing: 24,
    },
  } as KeenSliderOptions;

  const {
    currentRange,
    sliderRef,
    setPercentageValue,
    instanceRef,
    setCurrentRange,
  } = useSlider({
    config: DEFAULT_SLIDER_CONFIG,
    sliderMaxLength: SLIDER_MAX_LENGTH,
  });

  const paddingOffset = useMemo(() => {
    const padding = onDashboard ? (isMobile ? 2 : 3.5) : pagePadding;
    return padding * 8 * 2;
  }, [isMobile, pagePadding]);

  const refresh = () => {
    instanceRef.current?.update(instanceRef.current?.options);
    setCurrentRange(0);
    instanceRef.current?.track.to(0);
    instanceRef.current?.moveToIdx(0);
  };

  useEffect(() => {
    refresh();
  }, [filters, sorting, searchPhrase, voteTransaction?.proposalId, data]);

  const rangeSliderCalculationElement =
    dataLength < notSlicedDataLength
      ? (screenWidth +
          (onDashboard ? -290 - paddingOffset : -paddingOffset + 250)) /
        437
      : (screenWidth + (onDashboard ? -280 - paddingOffset : -paddingOffset)) /
        402;

  const handleLinkPress = useCallback(() => {
    if (onDashboard) {
      navigate(
        generatePath(PATHS.dashboardGovernanceActionsCategory, {
          category: navigateKey,
        }),
      );
    } else {
      navigate(
        generatePath(PATHS.governanceActionsCategory, {
          category: navigateKey,
        }),
      );
    }
  }, [onDashboard]);

  return (
    <Box>
      <Box display="flex" justifyContent="space-between" mb={3.5}>
        <Typography fontSize={16} lineHeight="24px" fontWeight={600}>
          {title}
        </Typography>
        {isMobile && isShowAll && (
          <Link
            data-testid={`show-all-link-${title.replace(/ /g, "")}`}
            sx={[
              {
                "&:hover": { cursor: "pointer" },
                textDecoration: "none",
                display: "flex",
              },
            ]}
            onClick={handleLinkPress}
          >
            <Typography
              color="primary"
              fontSize={14}
              fontWeight={500}
              lineHeight="20px"
              noWrap
            >
              {t("slider.showAll")}
            </Typography>
          </Link>
        )}
      </Box>
      <div
        ref={sliderRef}
        className="keen-slider"
        style={{ width: "100%", overflow: "visible" }}
      >
        {data}
        {!isMobile && isShowAll && dataLength < notSlicedDataLength && (
          <div
            className="keen-slider__slide"
            style={{
              display: "flex",
              flexDirection: "column",
              justifyContent: "center",
              overflow: "visible",
              width: "auto",
            }}
          >
            <Link
              data-testid={`view-all-link-${title.replace(/ /g, "")}`}
              sx={[
                {
                  "&:hover": { cursor: "pointer" },
                  textDecoration: "none",
                  display: "flex",
                },
              ]}
              onClick={handleLinkPress}
            >
              <Typography noWrap color="primary" fontSize={16} fontWeight={500}>
                {t("slider.viewAll")}
              </Typography>
              <img
                src={ICONS.arrowRightIcon}
                alt="arrow"
                style={{ marginLeft: "16px" }}
              />
            </Link>
          </div>
        )}
      </div>
      {!isMobile && Math.floor(rangeSliderCalculationElement) < dataLength && (
        <Box mt={3.5}>
          <input
            className={styles.rangeSlider}
            type="range"
            max={SLIDER_MAX_LENGTH}
            value={currentRange}
            onChange={setPercentageValue}
          />
        </Box>
      )}
    </Box>
  );
};
