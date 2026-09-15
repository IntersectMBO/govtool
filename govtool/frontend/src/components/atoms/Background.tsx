import { ReactNode, CSSProperties } from "react";
import { IMAGES } from "@consts";
import { useScreenDimension } from "@/hooks";

type BackgroundProps = {
  children: ReactNode;
  isReverted?: boolean;
  opacity?: number;
};

const getStyle = (
  isMobile: boolean,
  isReverted: boolean,
  type: "orange" | "blue",
  opacity: number,
): CSSProperties => {
  const commonStyles: CSSProperties = {
    opacity,
    position: "fixed",
    zIndex: -10,
  };

  let orangeBottomStyle = -650;
  if (isMobile) {
    orangeBottomStyle = -150;
    if (isReverted) {
      orangeBottomStyle = 200;
    }
  }

  let orangeRightStyle = -650;
  if (isMobile) {
    orangeRightStyle = -250;
    if (isReverted) {
      orangeRightStyle = 450;
    }
  }

  let blueTopStyle = -500;
  if (isMobile) {
    blueTopStyle = -150;
    if (isReverted) {
      blueTopStyle = 400;
    }
  }
  let blueLeftStyle = -400;
  if (isMobile) {
    blueLeftStyle = -250;
    if (isReverted) {
      blueLeftStyle = 400;
    }
  }

  const positions = {
    orange: {
      bottom: orangeBottomStyle,
      right: orangeRightStyle,
    },
    blue: {
      top: blueTopStyle,
      left: blueLeftStyle,
    },
  };

  return { ...commonStyles, ...positions[type] };
};

export const Background = ({
  children,
  isReverted = false,
  opacity = 1,
}: BackgroundProps) => {
  const { isMobile } = useScreenDimension();

  return (
    <>
      <img
        alt="bg-orange"
        height={isMobile ? 600 : "auto"}
        src={IMAGES.bgOrange}
        style={getStyle(isMobile, isReverted, "orange", opacity)}
        width={isMobile ? 600 : "auto"}
      />
      {children}
      <img
        alt="bg-blue"
        height={isMobile ? 600 : "auto"}
        src={IMAGES.bgBlue}
        style={getStyle(isMobile, isReverted, "blue", opacity)}
        width={isMobile ? 600 : "auto"}
      />
    </>
  );
};
