package org.cardano.govtool.actions;

import org.cardano.govtool.PdfApiService;
import org.cardano.govtool.feeders.RandomDataFeeder;

import io.gatling.javaapi.core.ChainBuilder;

import static io.gatling.javaapi.core.CoreDsl.*;

public class ProposalAction {

        public static ChainBuilder commentOnProposal = group("Comments").on(
                        feed(RandomDataFeeder.commentText).feed(RandomDataFeeder.randomJwt))
                        .exec(PdfApiService.postComments).exec(PdfApiService.getComments);

        public static ChainBuilder getPolls = group("Polls").on(
                        feed(RandomDataFeeder.randomJwt))
                        .exec(PdfApiService.getActivePolls)
                        .exec(PdfApiService.getInActivePolls);

        public static ChainBuilder getGovernanceActionType = group("Proposal").on(
                        feed(RandomDataFeeder.randomJwt))
                        .exec(PdfApiService.getGovernanceActionType);

        public static ChainBuilder voteOnProposal = group("Like/Dislike").on(
                        feed(RandomDataFeeder.proposalVote).feed(RandomDataFeeder.randomJwt))
                        .exec(PdfApiService.proposalVote);

        public static ChainBuilder createProposal = group("Create Proposal").on(
                        feed(RandomDataFeeder.title)).feed(RandomDataFeeder.abstractText)
                        .feed(RandomDataFeeder.motivationText)
                        .feed(RandomDataFeeder.rationaleText)
                        .feed(RandomDataFeeder.governanceActionTypeId).feed(RandomDataFeeder.proposalLink)
                        .feed(RandomDataFeeder.proposalLinkText)
                        .feed(RandomDataFeeder.randomBech32StakeAddress)
                        .feed(RandomDataFeeder.proposalAmount)
                        .feed(RandomDataFeeder.randomJwt)
                        .exec(PdfApiService.createProposalDraft)
                        .exec(PdfApiService.createProposal);
}
