import { ReactNode } from "react";

import { IMAGES } from "@consts";
import { useScreenDimension } from "@/hooks";

export const Background = ({
  children,
  isReverted = false,
  opacity = 1,
}: {
  children: ReactNode;
  isReverted?: boolean;
  opacity?: number;
}) => {
  const { isMobile } = useScreenDimension();

  return (
    <>
      <img
        alt="bg-orange"
        height={isMobile ? 600 : "auto"}
        src={IMAGES.bgOrange}
        style={{
          bottom: isMobile ? -150 : isReverted ? 200 : -650,
          opacity,
          position: "fixed",
          right: isMobile ? -250 : isReverted ? 450 : -650,
          zIndex: -10,
        }}
        width={isMobile ? 600 : "auto"}
      />
      {children}
      <img
        alt="bg-blue"
        height={isMobile ? 600 : "auto"}
        src={IMAGES.bgBlue}
        style={{
          left: isMobile ? -250 : isReverted ? 400 : -400,
          opacity,
          position: "fixed",
          top: isMobile ? -150 : isReverted ? 400 : -500,
          zIndex: -10,
        }}
        width={isMobile ? 600 : "auto"}
      />
    </>
  );
};
