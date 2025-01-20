package org.cardano.govtool.actions;

import org.cardano.govtool.ApiService;
import org.cardano.govtool.feeders.RandomDataFeeder;

import io.gatling.javaapi.core.ChainBuilder;

import static io.gatling.javaapi.core.CoreDsl.*;

public class ProposalAction {

    public static ChainBuilder commentOnProposal = group("Comment").on(
            feed(RandomDataFeeder.commentText))
            .exec(ApiService.postComments);

    public static ChainBuilder voteOnProposal = group("Like/Dislike").on(
            feed(RandomDataFeeder.proposalVote))
            .exec(ApiService.proposalVote);

    public static ChainBuilder createProposalDraft = group("Create Proposal").on(
            feed(RandomDataFeeder.title)).feed(RandomDataFeeder.abstractText).feed(RandomDataFeeder.motivationText)
            .feed(RandomDataFeeder.rationaleText)
            .feed(RandomDataFeeder.governanceActionTypeId).feed(RandomDataFeeder.proposalLink)
            .feed(RandomDataFeeder.proposalLinkText)
            .exec(ApiService.createProposalDraft);

    public static ChainBuilder createInfoProposal = group("Create Proposal").on(
            feed(RandomDataFeeder.title)).feed(RandomDataFeeder.abstractText).feed(RandomDataFeeder.motivationText)
            .feed(RandomDataFeeder.rationaleText)
            .feed(RandomDataFeeder.proposalLink)
            .feed(RandomDataFeeder.proposalLinkText)
            .exec(ApiService.createProposal);
}
