import { KeenSliderHooks, KeenSliderInstance } from "keen-slider/react";
import { SliderArrow } from "@atoms";
import { Box } from "@mui/material";

type SliderArrowsProps = {
  currentSlide: number;
  instanceRef: React.MutableRefObject<KeenSliderInstance<
    object,
    object,
    KeenSliderHooks
  > | null>;
  itemsPerView: number;
};

export const SliderArrows = ({
  currentSlide,
  instanceRef,
  itemsPerView,
}: SliderArrowsProps) => (
  <>
    {instanceRef.current && (
      <Box
        sx={{
          display: "flex",
          gap: "4px",
        }}
      >
        <SliderArrow
          left
          onClick={(e: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
            e.stopPropagation();
            instanceRef.current?.prev();
          }}
          disabled={currentSlide === 0}
        />
        <SliderArrow
          onClick={(e: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
            e.stopPropagation();
            instanceRef.current?.next();
          }}
          disabled={
            currentSlide + itemsPerView >=
            instanceRef.current.track.details.slides.length
          }
        />
      </Box>
    )}
  </>
);
