import { KeenSliderHooks, KeenSliderInstance } from "keen-slider/react";
import { SliderArrow } from "@atoms";
import { Box } from "@mui/material";

interface ArrowsProps {
  currentSlide: number;
  instanceRef: React.MutableRefObject<KeenSliderInstance<
    object,
    object,
    KeenSliderHooks
  > | null>;
}

export const SliderArrows = ({ currentSlide, instanceRef }: ArrowsProps) => (
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
            currentSlide === instanceRef.current.track.details.slides.length - 1
          }
        />
      </Box>
    )}
  </>
);
