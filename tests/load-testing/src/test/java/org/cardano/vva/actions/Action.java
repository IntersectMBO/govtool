package org.cardano.vva.actions;

import io.gatling.javaapi.core.ChainBuilder;
import org.cardano.vva.ApiService;

import static io.gatling.javaapi.core.CoreDsl.*;

// Common Actions
public class Action {
    public static ChainBuilder viewProposals = group("View Proposals").on(exec(ApiService.getAllProposals)
            .exec(ApiService.getVotes));

    public static ChainBuilder viewVotes = exec(ApiService.getVotes);
}
