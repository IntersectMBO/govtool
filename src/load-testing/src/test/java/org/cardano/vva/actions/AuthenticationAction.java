package org.cardano.vva.actions;

import io.gatling.javaapi.core.ChainBuilder;
import org.cardano.vva.ApiService;
import org.cardano.vva.feeders.RandomDataFeeder;

import java.util.UUID;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.poll;

public class AuthenticationAction {
    public static ChainBuilder connect = group("Login")
            .on(exec(ApiService.getAllDReps)
                    .feed(RandomDataFeeder.stakeKey)
                    .exec(ApiService.getCurrentDelegation)
                    .exec(ApiService.getParams)
                    .feed(RandomDataFeeder.dRepId)
                    .exec(ApiService.getDRepVotingPower)
                    .exec(ApiService.getAdaHolderVotingPower));
}
