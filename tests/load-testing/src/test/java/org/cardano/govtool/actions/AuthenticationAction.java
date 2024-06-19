package org.cardano.govtool.actions;

import io.gatling.javaapi.core.ChainBuilder;
import org.cardano.govtool.ApiService;
import org.cardano.govtool.feeders.RandomDataFeeder;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static io.gatling.javaapi.core.CoreDsl.*;

public class AuthenticationAction {
    public static ChainBuilder connect (List<String> knownDrepIds) {

        var drepStream = Stream.generate((Supplier<Map<String, Object>>) () -> {
                     var randomIndex=ThreadLocalRandom.current().nextInt(0,knownDrepIds.size());
                     var dRepKey =  knownDrepIds.get(randomIndex);
                    return Collections.singletonMap("dRepId", dRepKey);
                }
        ).iterator();

        return group("Login")
                .on(feed(RandomDataFeeder.stakeKey)
                        .feed(drepStream)
                        .exec(ApiService.getCurrentDelegation)
                        .exec(ApiService.getParams)
                        .exec(ApiService.getDRepVotingPower)
                        .exec(ApiService.getAdaHolderVotingPower));
    }
}
