import { useEffect, useState } from "react";

/** True once `ms` have passed since the component mounted. */
export const useTimedOut = (ms: number) => {
  const [isOut, setIsOut] = useState(false);
  useEffect(() => {
    const id = window.setTimeout(() => setIsOut(true), ms);
    return () => window.clearTimeout(id);
  }, [ms]);
  return isOut;
};
