package org.cardano.govtool;

import io.gatling.javaapi.core.ChainBuilder;
import static io.gatling.javaapi.core.CoreDsl.*;

import static io.gatling.javaapi.http.HttpDsl.http;

public class PdfApiService {

    public static ChainBuilder getComments = exec(
            http("get_comments").get("/api/comments").queryParam("filters[$and][0][proposal_id]", "#{proposalDiscussionId}")
                    .headers(Utils.proposalDefaultHeaders("#{jwt}")));

    public static ChainBuilder postComments = exec(http("add_comments_on_proposal").post("/api/comments")
            .body(StringBody(
                    "{\"data\":{\"proposal_id\":\"#{proposalDiscussionId}\",\"comment_text\":\"#{commentText}\"}}"))
            .headers(Utils.proposalDefaultHeaders("#{jwt}")));

    public static ChainBuilder getActivePolls = exec(http("get_proposal_active_polls").get("/api/polls")
            .queryParam("filters[$and][0][proposal_id]", "#{proposalDiscussionId}")
            .queryParam("filters[$and][1][is_poll_active]", "true").headers(Utils.proposalDefaultHeaders("#{jwt}")));

    public static ChainBuilder getInActivePolls = exec(http("get_proposal_inactive_polls").get("/api/polls")
            .queryParam("filters[$and][0][proposal_id]", "#{proposalDiscussionId}")
            .queryParam("filters[$and][1][is_poll_active]", "false").headers(Utils.proposalDefaultHeaders("#{jwt}")));

    public static ChainBuilder proposalVote = exec(http("like/dislike_on_proposals").post("/api/proposal-votes")
            .body(StringBody(
                    "{\"data\":{\"proposal_id\":\"#{proposalDiscussionId}\",\"vote_result\":#{proposalVote}}}"))
            .headers(Utils.proposalDefaultHeaders("#{jwt}")));

    public static ChainBuilder getGovernanceActionType = exec(http("get_gov_action_type")
            .get("/api/governance-action-types").headers(Utils.proposalDefaultHeaders("#{jwt}")));

    public static ChainBuilder createProposalDraft = exec(http("create_proposal_draft").post("/api/proposals")
            .body(StringBody(
                    "{\"data\":{\"proposal_links\":[{\"prop_link\":\"#{proposalLink}\",\"prop_link_text\":\"#{proposalLinkText}\"}],\"proposal_withdrawals\":[{\"prop_receiving_address\":\"#{stakeAddress}\",\"prop_amount\":\"#{proposalAmount}\"}],\"gov_action_type_id\":#{governanceActionTypeId},\"prop_name\":\"#{title}\",\"prop_abstract\":\"#{abstract}\",\"prop_motivation\":\"#{motivation}\",\"prop_rationale\":\"#{rationale}\",\"is_draft\":true}}"))
            .headers(Utils.proposalDefaultHeaders("#{jwt}")));

    public static ChainBuilder createProposal = exec(http("create_proposal").post("/api/proposals").body(StringBody(
            "{\"data\":{\"proposal_links\":[{\"prop_link\":\"#{proposalLink}\",\"prop_link_text\":\"#{proposalLinkText}\"}],\"proposal_withdrawals\":[{\"prop_receiving_address\":\"#{stakeAddress}\",\"prop_amount\":\"#{proposalAmount}\"}],\"gov_action_type_id\":#{governanceActionTypeId},\"prop_name\":\"#{title}\",\"prop_abstract\":\"#{abstract}\",\"prop_motivation\":\"#{motivation}\",\"prop_rationale\":\"#{rationale}\",\"is_draft\":false}}"))
            .headers(Utils.proposalDefaultHeaders("#{jwt}")));
}
