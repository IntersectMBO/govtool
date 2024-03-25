import { ChangeEvent, useState } from "react";
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
  sliderMaxLength,
}: {
  config: KeenSliderOptions;
  sliderMaxLength: number;
}) => {
  const [currentSlide, setCurrentSlide] = useState(0);
  const [currentRange, setCurrentRange] = useState(0);

  const [sliderRef, instanceRef] = useKeenSlider(
    {
      ...config,
      rubberband: false,
      detailsChanged: (slider) => {
        setCurrentRange(slider.track.details.progress * sliderMaxLength);
        setCurrentSlide(slider.track.details.rel);
      },
    },
    [WheelControls],
  );

  const DATA_LENGTH = instanceRef?.current?.slides?.length ?? 10;
  const ITEMS_PER_VIEW =
    DATA_LENGTH - (instanceRef?.current?.track?.details?.maxIdx ?? 2);

  const setPercentageValue = (e: ChangeEvent<HTMLInputElement>) => {
    const target = e?.target;
    const currentIndexOfSlide = Math.floor(
      +(target?.value ?? 0) /
        (sliderMaxLength / (DATA_LENGTH - Math.floor(ITEMS_PER_VIEW))),
    );

    instanceRef.current?.track.add(
      (+(target?.value ?? 0) - currentRange) *
        (instanceRef.current.track.details.length / sliderMaxLength),
    );
    setCurrentRange(+(target?.value ?? 0));
    setCurrentSlide(currentIndexOfSlide);
  };

  return {
    sliderRef,
    instanceRef,
    currentSlide,
    currentRange,
    setCurrentRange,
    setPercentageValue,
  };
};
