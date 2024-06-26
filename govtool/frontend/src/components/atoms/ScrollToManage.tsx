/* eslint-disable @typescript-eslint/no-explicit-any */
import { useEffect } from "react";
import { useLocation } from "react-router-dom";
import { PATHS } from "@/consts";

export function debounce(
  fn: (...params: any) => void,
  wait: number,
): (...params: any) => void {
  let timer: any = null;
  return (...params: any) => {
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
        pathname === PATHS.dashboardGovernanceActions ||
        pathname === PATHS.governanceActions
      ) {
        pathMap.set(pathname, 0);
      }
      window.scrollTo(0, 0);
    }
  }, [pathname]);

  useEffect(() => {
    const fn = debounce(() => {
      if (
        pathname === PATHS.dashboardGovernanceActions ||
        pathname === PATHS.governanceActions
      ) {
        pathMap.set(pathname, window.scrollY);
      }
    }, 200);

    window.addEventListener("scroll", fn);
    return () => window.removeEventListener("scroll", fn);
  }, [pathname]);

  return <></>;
};
