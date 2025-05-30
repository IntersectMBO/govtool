import {
  createContext,
  createRef,
  useContext,
  useMemo,
  useRef,
  useState,
  useLayoutEffect,
  useEffect,
} from "react";
import {
  getItemFromLocalStorage,
  MAINTENANCE_ENDING_BANNER_EXPANDED_KEY,
  setItemToLocalStorage,
} from "@/utils";

interface MaintenanceEndingBannerContextType {
  ref: React.RefObject<HTMLDivElement>;
  height: number;
  isExpanded: boolean;
  toggleExpanded: () => void;
}

const MaintenanceEndingBannerContext =
  createContext<MaintenanceEndingBannerContextType>({
    ref: createRef<HTMLDivElement>(),
    height: 0,
    isExpanded: true,
    toggleExpanded: () => {},
  });

export const MaintenanceEndingBannerProvider = ({
  children,
}: React.PropsWithChildren) => {
  const ref = useRef<HTMLDivElement>(null);
  const [height, setHeight] = useState(0);
  const [isExpanded, setIsExpanded] = useState<boolean>(true);

  useEffect(() => {
    const storedValue = getItemFromLocalStorage(
      MAINTENANCE_ENDING_BANNER_EXPANDED_KEY,
    );
    if (storedValue !== null) {
      setIsExpanded(JSON.parse(storedValue));
    }
  }, []);

  const toggleExpanded = () => {
    setItemToLocalStorage(MAINTENANCE_ENDING_BANNER_EXPANDED_KEY, !isExpanded);
    setIsExpanded((prev) => !prev);
  };

  useLayoutEffect(() => {
    let frameId: number | null = null;

    const updatePosition = () => {
      if (ref.current) {
        const rect = ref.current.getBoundingClientRect();

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
      isExpanded,
      toggleExpanded,
    }),
    [ref, height, isExpanded, toggleExpanded],
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
