/* eslint-disable func-names */
/* eslint-disable no-multi-assign */
/* eslint-disable prefer-template */
/* eslint-disable no-underscore-dangle */
/* eslint-disable wrap-iife */
import { useEffect } from "react";

export const useMatomo = () => {
  useEffect(() => {
    const isSancho = window.location.href.includes("sancho");
    if (isSancho) return;

    const network = import.meta.env.VITE_NETWORK_FLAG;
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-expect-error
    const _paq = (window._paq = window._pag || []);
    _paq.push(["trackPageView"]);
    _paq.push(["enableLinkTracking"]);
    (function () {
      const u = "//analytics.gov.tools/";
      _paq.push(["setTrackerUrl", u + "matomo.php"]);
      _paq.push(["setSiteId", network === 1 ? "1" : "2"]);
      const d = document;
      const g = d.createElement("script");
      const s = d.getElementsByTagName("script")[0];
      g.async = true;
      g.src = u + "matomo.js";
      s.parentNode?.insertBefore(g, s);
    })();
  }, []);
};
