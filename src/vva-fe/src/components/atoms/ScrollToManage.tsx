import { PATHS } from "@/consts";
import { useEffect } from "react";
import { useLocation } from "react-router-dom";

export function debounce(
  fn: (...params: any) => void,
  wait: number
): (...params: any) => void {
  let timer: any = null;
  return function (...params: any) {
    clearTimeout(timer);
    timer = setTimeout(() => {
      fn(...params);
    }, wait);
  };
}

export const pathMap = new Map<string, number>();

export const ScrollToManage = () => {
  const { pathname } = useLocation();

  useEffect(() => {
    if (pathMap.has(pathname)) {
      window.scrollTo(0, pathMap.get(pathname)!);
    } else {
      if (
        pathname === PATHS.dashboard_governance_actions ||
        pathname === PATHS.governance_actions
      ) {
        pathMap.set(pathname, 0);
      }
      window.scrollTo(0, 0);
    }
  }, [pathname]);

  useEffect(() => {
    const fn = debounce(() => {
      if (
        pathname === PATHS.dashboard_governance_actions ||
        pathname === PATHS.governance_actions
      ) {
        pathMap.set(pathname, window.scrollY);
      }
    }, 200);

    window.addEventListener("scroll", fn);
    return () => window.removeEventListener("scroll", fn);
  }, [pathname]);

  return <></>;
};
