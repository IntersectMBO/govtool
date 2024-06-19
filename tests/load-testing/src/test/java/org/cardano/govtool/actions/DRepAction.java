package org.cardano.govtool.actions;

import org.cardano.govtool.ApiService;
import org.cardano.govtool.feeders.RandomDataFeeder;
import io.gatling.javaapi.core.ChainBuilder;

import static io.gatling.javaapi.core.CoreDsl.*;
import static org.cardano.govtool.feeders.PageVisits.visitProposalPage;

public class DRepAction {
    public static ChainBuilder registerAsDRep = group("RegisterAsDRep").on(
            feed(RandomDataFeeder.txId).exec(ApiService.getTxStatus)
                    .exec(ApiService.validate_sample_metadata)
                    .pause(4)
                    .exec(ApiService.pollTxStatus)
                    .exec(ApiService.getAllDReps)
    );


    public static ChainBuilder retireAsDRep = group("RetireAsDRep").on(
            feed(RandomDataFeeder.txId).exec(ApiService.getTxStatus)
                    .pause(4)
                    .exec(ApiService.pollTxStatus)
                    .exec(ApiService.getAllDReps));
    public static ChainBuilder vote = group("Voting").on(
            feed(RandomDataFeeder.txId)
                    .feed(RandomDataFeeder.dRepId)
                    .feed(RandomDataFeeder.proposalId)
                    .exec(visitProposalPage())
                    .pause(4)
                    // viewing propsals refetches the entire proposal list
                    .exec(repeat(3).on(visitProposalPage()))
                    .pause(4)
                    .exec(ApiService.getTxStatus)
                    .pause(4)
                    .exec(ApiService.pollTxStatus)
                    .exec(visitProposalPage()));

}
