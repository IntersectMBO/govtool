import { Navigate, useLocation } from "react-router";

import { getCurrentPath } from "@utils";

import { ErrorPage } from "./ErrorPage";

// Sends old underscore URLs (bookmarks, shared links) to their current path,
// keeping the query and hash; anything else is a 404.
export const LegacyPathRedirect = () => {
  const { pathname, search, hash } = useLocation();
  const currentPath = getCurrentPath(pathname);

  if (!currentPath) return <ErrorPage />;

  return <Navigate to={{ pathname: currentPath, search, hash }} replace />;
};
