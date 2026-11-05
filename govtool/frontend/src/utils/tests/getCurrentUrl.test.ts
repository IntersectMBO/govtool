import { describe, expect, it } from "vitest";

import { getCurrentUrl } from "../getCurrentUrl";
import { getGovActionCIP129Id } from "../getGovActionId";

const TX = "ab".repeat(32);
const url = (pathname: string, search = "", hash = "") => ({
  pathname,
  search,
  hash,
});

describe("getCurrentUrl", () => {
  it("renames underscore paths", () => {
    expect(getCurrentUrl(url("/drep_directory"))).toEqual(url("/dreps"));
    expect(getCurrentUrl(url("/budget_discussion/12"))).toEqual(
      url("/budget/12"),
    );
    expect(
      getCurrentUrl(url("/proposal_discussion/proposal_comment_review/h")),
    ).toEqual(url("/proposals/comment-review/h"));
    expect(getCurrentUrl(url("/my/votes_and_favorites"))).toEqual(
      url("/my/votes"),
    );
  });

  it("moves intents to /new and /my", () => {
    expect(getCurrentUrl(url("/create_governance_action"))).toEqual(
      url("/actions/new"),
    );
    expect(getCurrentUrl(url("/proposal_discussion/propose"))).toEqual(
      url("/proposals/new"),
    );
    expect(getCurrentUrl(url("/register_drep"))).toEqual(
      url("/my/drep/register"),
    );
    expect(getCurrentUrl(url("/edit_drep"))).toEqual(url("/my/drep/edit"));
    expect(getCurrentUrl(url("/retire_direct_voter"))).toEqual(
      url("/my/direct-voter/retire"),
    );
    expect(getCurrentUrl(url("/stake_keys"))).toEqual(url("/my/stake-key"));
  });

  it("matches old segments in any case, as the router did", () => {
    expect(getCurrentUrl(url("/edit_dRep"))).toEqual(url("/my/drep/edit"));
    expect(getCurrentUrl(url("/Connected/DRep_Directory/drep1Ab"))).toEqual(
      url("/dreps/drep1Ab"),
    );
    expect(
      getCurrentUrl(url("/governance_actions/Category/InfoAction")),
    ).toEqual(url("/actions", "?type=InfoAction"));
  });

  it("drops /connected, keeping the query", () => {
    expect(
      getCurrentUrl(url("/connected/drep_directory/drep1abc", "?q=1")),
    ).toEqual(url("/dreps/drep1abc", "?q=1"));
    expect(getCurrentUrl(url("/connected/dreps"))).toEqual(url("/dreps"));
  });

  it("turns a category into a type filter", () => {
    expect(
      getCurrentUrl(url("/connected/governance_actions/category/InfoAction")),
    ).toEqual(url("/actions", "?type=InfoAction"));
    expect(
      getCurrentUrl(url("/actions/category/Info%20Action", "?a=b")),
    ).toEqual(url("/actions", "?a=b&type=Info+Action"));
  });

  it("names an action by its CIP-129 id", () => {
    const id = getGovActionCIP129Id(TX, 1);
    expect(getCurrentUrl(url(`/governance_actions/${TX}`, "", "#1"))).toEqual(
      url(`/actions/${id}`),
    );
    expect(
      getCurrentUrl(
        url(`/governance_actions/category/InfoAction/${TX}`, "", "#1"),
      ),
    ).toEqual(url(`/actions/${id}`));
    expect(getCurrentUrl(url(`/actions/history/${TX}`, "", "#1"))).toEqual(
      url(`/actions/history/${id}`),
    );
    expect(getCurrentUrl(url(`/actions/${TX}`))).toEqual(
      url(`/actions/${getGovActionCIP129Id(TX, 0)}`),
    );
  });

  it("leaves current and unknown URLs alone", () => {
    expect(getCurrentUrl(url("/"))).toBeNull();
    expect(getCurrentUrl(url("/dreps", "?search=x", "#top"))).toBeNull();
    expect(
      getCurrentUrl(url(`/actions/${getGovActionCIP129Id(TX, 0)}`)),
    ).toBeNull();
    expect(getCurrentUrl(url("/actions", "?type=InfoAction"))).toBeNull();
    expect(getCurrentUrl(url("/budget/category/general"))).toBeNull();
    expect(getCurrentUrl(url("/no_such_page"))).toBeNull();
    expect(getCurrentUrl(url("/constructor"))).toBeNull();
  });
});
