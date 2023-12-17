package org.cardano.vva.actions;

import org.cardano.vva.ApiService;
import org.cardano.vva.configs.HeaderConfig;
import io.gatling.javaapi.core.ChainBuilder;
import org.cardano.vva.feeders.RandomDataFeeder;

import static io.gatling.javaapi.core.CoreDsl.*;

public class AdaHolderAction {
    public static ChainBuilder delegateToDRep = group("Delegation").on(exec(ApiService.getAllDReps)
            .pause(6)
            .feed(RandomDataFeeder.txId)
            .exec(ApiService.getTxStatus)
            .pause(4)
            .exec(ApiService.pollTxStatus)
            .feed(RandomDataFeeder.stakeKey)
            .exec(ApiService.getCurrentDelegation));
}
