import {
  createContext,
  createRef,
  useContext,
  useMemo,
  useRef,
  useState,
  useLayoutEffect,
} from "react";

interface MaintenanceEndingBannerContextType {
  ref: React.RefObject<HTMLDivElement>;
  height: number;
  y: number;
  isExpanded: boolean;
  toggleExpanded: () => void;
}

const MaintenanceEndingBannerContext =
  createContext<MaintenanceEndingBannerContextType>({
    ref: createRef<HTMLDivElement>(),
    height: 0,
    y: 0,
    isExpanded: true,
    toggleExpanded: () => {},
  });

export const MaintenanceEndingBannerProvider = ({
  children,
}: React.PropsWithChildren) => {
  const ref = useRef<HTMLDivElement>(null);
  const [height, setHeight] = useState(0);
  const [y, setY] = useState(0);
  const [isExpanded, setIsExpanded] = useState(true);

  const toggleExpanded = () => {
    setIsExpanded((prev) => !prev);
  };

  useLayoutEffect(() => {
    let frameId: number | null = null;

    const updatePosition = () => {
      if (ref.current) {
        const rect = ref.current.getBoundingClientRect();

        setY(rect.top);
        setHeight(rect.height);
      }
    };

    const throttledUpdate = () => {
      // Context skipping update - frame already queued
      if (frameId) return;

      frameId = requestAnimationFrame(() => {
        updatePosition();
        frameId = null;
      });
    };

    updatePosition();

    window.addEventListener("scroll", throttledUpdate, { passive: true });
    window.addEventListener("resize", throttledUpdate);

    return () => {
      window.removeEventListener("scroll", throttledUpdate);
      window.removeEventListener("resize", throttledUpdate);

      if (frameId) {
        cancelAnimationFrame(frameId);
      }
    };
  }, []);

  const value = useMemo(
    () => ({
      ref,
      height,
      y,
      isExpanded,
      toggleExpanded,
    }),
    [ref, height, y, isExpanded, toggleExpanded],
  );

  return (
    <MaintenanceEndingBannerContext.Provider value={value}>
      {children}
    </MaintenanceEndingBannerContext.Provider>
  );
};

export const useMaintenanceEndingBannerContext = () => {
  const context = useContext(MaintenanceEndingBannerContext);
  if (!context) {
    throw new Error(
      "useMaintenanceEndingBannerContext must be used within a MaintenanceEndingBannerProvider",
    );
  }
  return context;
};
