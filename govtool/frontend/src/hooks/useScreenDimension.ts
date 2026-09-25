import { useMemo, useSyncExternalStore } from "react";

/**
 * Window width, shared by every component that asks for it.
 *
 * One `resize` listener for the whole app, throttled to one update per
 * animation frame and fired only when the width actually changed. Each caller
 * used to add its own listener and set its own state on every resize event,
 * which re-rendered dozens of components separately while a window was
 * dragged.
 */
let width = typeof window === "undefined" ? 0 : window.innerWidth;
const listeners = new Set<() => void>();
let frame = 0;

const onResize = () => {
  if (frame) return;
  frame = window.requestAnimationFrame(() => {
    frame = 0;
    const next = window.innerWidth;
    if (next === width) return;
    width = next;
    listeners.forEach((listener) => listener());
  });
};

const subscribe = (listener: () => void) => {
  if (listeners.size === 0) {
    width = window.innerWidth;
    window.addEventListener("resize", onResize);
  }
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
    if (listeners.size === 0) {
      window.removeEventListener("resize", onResize);
      if (frame) window.cancelAnimationFrame(frame);
      frame = 0;
    }
  };
};

const getWidth = () => width;

const paddingFor = (screenWidth: number) => {
  if (screenWidth < 768) return 2;
  if (screenWidth < 1024) return 6;
  if (screenWidth < 1440) return 8;
  if (screenWidth < 1920) return 10;
  return 37;
};

export const useScreenDimension = () => {
  const screenWidth = useSyncExternalStore(subscribe, getWidth, getWidth);
  return useMemo(
    () => ({
      screenWidth,
      isMobile: screenWidth < 768,
      pagePadding: paddingFor(screenWidth),
    }),
    [screenWidth],
  );
};
