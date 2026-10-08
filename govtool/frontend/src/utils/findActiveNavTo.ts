/**
 * The one link of a group that matches the current path. A NavLink is active
 * on its own path and every path below it, so /governance_actions (Live
 * Voting) would also match /governance_actions/history; the longest match
 * wins, and links to "" or to other sites never match.
 */
export const findActiveNavTo = (
  pathname: string,
  navTos: Array<string | null | undefined>,
): string | null =>
  navTos.reduce<string | null>((active, navTo) => {
    if (!navTo || !navTo.startsWith("/")) return active;
    const base = navTo.replace(/\/+$/, "") || "/";
    const matches =
      pathname === base || pathname.startsWith(base === "/" ? "/" : `${base}/`);
    if (!matches) return active;
    return !active || base.length > active.length ? navTo : active;
  }, null);
