import { PATHS, USER_PATHS } from "@/consts/paths";

/**
 * Whether a page needs a wallet: the dashboard, the …/new forms and the
 * wallet's own pages under /my. Every other page has a public layout.
 */
export const isWalletOnlyPath = (pathname: string) => {
  const path = pathname.toLowerCase().replace(/\/+$/, "");
  return (
    path === PATHS.dashboard ||
    path.endsWith("/new") ||
    (path.startsWith("/my/") && path !== USER_PATHS.governanceActionsVotedByMe)
  );
};
