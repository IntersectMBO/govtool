import { useCallback, useEffect, useRef, useState } from "react";

/**
 * A whole-second countdown. It tracks a deadline rather than decrementing, so a
 * throttled background tab still reaches zero on time.
 */
export const useCountdown = () => {
  const [remaining, setRemaining] = useState(0);
  const deadline = useRef<number | null>(null);

  const start = useCallback((seconds: number) => {
    const whole = Math.max(0, Math.ceil(seconds));
    deadline.current = whole > 0 ? Date.now() + whole * 1000 : null;
    setRemaining(whole);
  }, []);

  const isRunning = remaining > 0;

  useEffect(() => {
    if (!isRunning || deadline.current === null) return undefined;
    const id = setInterval(() => {
      const left = Math.max(
        0,
        Math.ceil(((deadline.current ?? 0) - Date.now()) / 1000),
      );
      setRemaining(left);
      if (left === 0) deadline.current = null;
    }, 1000);
    return () => clearInterval(id);
  }, [isRunning]);

  return { remaining, isRunning, start };
};
