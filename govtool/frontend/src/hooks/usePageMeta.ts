import { useEffect, useRef } from "react";
import { useLocation } from "react-router";

import I18n from "@/i18n";
import { getPageTitle } from "@/utils/getPageTitle";

/** A data-seo element in <head>, created when the server wrote none. */
const seoElement = (selector: string, create: () => HTMLElement) => {
  const existing = document.head.querySelector<HTMLElement>(selector);
  if (existing) return existing;
  const element = create();
  element.setAttribute("data-seo", "");
  document.head.appendChild(element);
  return element;
};

/**
 * Keeps the page's tags in step with in-app navigation. The first page's
 * tags come from the server (nginx includes them from the backend) and are
 * left exactly as they are, so what crawlers render matches what they
 * fetched. After that, the title, description and canonical are updated in
 * place; the canonical matters to people too, since Chrome on Android shares
 * it. React's own <title> rendering is not used, as it would add a second
 * set beside the server's.
 */
export const usePageMeta = () => {
  const { pathname, search, hash } = useLocation();
  const firstPath = useRef<string | null>(pathname);

  useEffect(() => {
    if (firstPath.current === pathname) return;
    firstPath.current = null;

    document.title =
      pathname === "/"
        ? I18n.t("seo.homeTitle")
        : `${getPageTitle(pathname, search)}${I18n.t("seo.titleSuffix")}`;

    seoElement('meta[name="description"][data-seo]', () => {
      const meta = document.createElement("meta");
      meta.setAttribute("name", "description");
      return meta;
    }).setAttribute("content", I18n.t("seo.siteDescription"));

    // A #fragment can carry what the path does not, such as an old link's
    // action index (/governance_actions/<txHash>#1). No canonical then, so a
    // browser shares the whole URL rather than one that opens another action.
    if (hash !== "") {
      document.head.querySelector('link[rel="canonical"][data-seo]')?.remove();
    } else {
      seoElement('link[rel="canonical"][data-seo]', () => {
        const link = document.createElement("link");
        link.setAttribute("rel", "canonical");
        return link;
      }).setAttribute("href", `${window.location.origin}${pathname}`);
    }

    document.head.querySelector('meta[name="robots"][data-seo]')?.remove();
  }, [pathname, search, hash]);
};
