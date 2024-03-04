import { KeenSliderHooks, KeenSliderInstance } from "keen-slider/react";
import { SliderArrow } from "@atoms";
import { Box } from "@mui/material";

interface ArrowsProps {
  currentSlide: number;
  instanceRef: React.MutableRefObject<KeenSliderInstance<
    {},
    {},
    KeenSliderHooks
  > | null>;
}

export const SliderArrows = ({ currentSlide, instanceRef }: ArrowsProps) => {
  return (
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
            onClick={(e: any) =>
              e.stopPropagation() || instanceRef.current?.prev()
            }
            disabled={currentSlide === 0}
          />
          <SliderArrow
            onClick={(e: any) =>
              e.stopPropagation() || instanceRef.current?.next()
            }
            disabled={
              currentSlide ===
              instanceRef.current.track.details.slides.length - 1
            }
          />
        </Box>
      )}
    </>
  );
};
