/* eslint-disable @typescript-eslint/no-explicit-any */
import { useEffect } from "react";
import { useLocation } from "react-router";
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
  const { pathname, search } = useLocation();
  // The list and one type's list (?type=…) share a path but not a position.
  const page = `${pathname}${search}`;

  useEffect(() => {
    if (pathMap.has(page)) {
      window.scrollTo(0, pathMap.get(page)!);
    } else {
      if (page === PATHS.governanceActions) {
        pathMap.set(page, 0);
      }
      window.scrollTo(0, 0);
    }
  }, [page]);

  useEffect(() => {
    const fn = debounce(() => {
      if (page === PATHS.governanceActions) {
        pathMap.set(page, window.scrollY);
      }
    }, 200);

    window.addEventListener("scroll", fn);
    return () => window.removeEventListener("scroll", fn);
  }, [page]);

  return <></>;
};
