import { useState } from "react";
import { KeenSliderOptions, useKeenSlider } from "keen-slider/react";
import type { KeenSliderInstance } from "keen-slider";

const WheelControls = (slider: KeenSliderInstance) => {
  let touchTimeout: NodeJS.Timeout;
  let position: { x: number; y: number };
  let wheelActive: boolean = false;

  function dispatch(e: WheelEvent, name: string) {
    position.x -= e.deltaX;
    position.y -= e.deltaY;
    slider.container.dispatchEvent(
      new CustomEvent(name, {
        detail: {
          x: position.x,
          y: position.y,
        },
      }),
    );
  }

  function eventWheel(e: WheelEvent) {
    if (Math.abs(e.deltaX) > Math.abs(e.deltaY)) {
      e.preventDefault();
      if (!wheelActive) {
        position = {
          x: e.pageX,
          y: e.pageY,
        };
        dispatch(e, "ksDragStart");
        wheelActive = true;
      }
      dispatch(e, "ksDrag");
      clearTimeout(touchTimeout);
      touchTimeout = setTimeout(() => {
        wheelActive = false;
        dispatch(e, "ksDragEnd");
      }, 50);
    }
  }

  slider.on("created", () => {
    slider.container.addEventListener("wheel", eventWheel, {
      passive: false,
    });
  });
};

export const useSlider = ({
  config,
}: {
  config: KeenSliderOptions;
  sliderMaxLength: number;
}) => {
  const [currentSlide, setCurrentSlide] = useState(0);

  const [sliderRef, instanceRef] = useKeenSlider(
    {
      ...config,
      rubberband: false,
      detailsChanged: (slider) => {
        setCurrentSlide(slider.track.details.rel);
      },
    },
    [WheelControls],
  );

  return {
    sliderRef,
    instanceRef,
    currentSlide,
  };
};
