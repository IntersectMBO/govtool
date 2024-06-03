package org.cardano.govtool.actions;

import io.gatling.javaapi.core.ChainBuilder;
import org.cardano.govtool.ApiService;
import org.cardano.govtool.feeders.PageVisits;

import static io.gatling.javaapi.core.CoreDsl.*;

// Common Actions
public class Action {
    public static ChainBuilder viewProposals = group("View Proposals").on(exec(PageVisits.visitProposalPage())
            .exec(ApiService.getVotes));

}
