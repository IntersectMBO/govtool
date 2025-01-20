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
    public static ChainBuilder connect (List<String> knownDrepIds,List<String> knownProposaList) {

        var drepStream = Stream.generate((Supplier<Map<String, Object>>) () -> {
                     var randomIndex=ThreadLocalRandom.current().nextInt(0,knownDrepIds.size());
                     var dRepKey =  knownDrepIds.get(randomIndex);
                    return Collections.singletonMap("dRepId", dRepKey);
                }
        ).iterator();

        var proposalListStream = Stream.generate((Supplier<Map<String, Object>>) () -> {
            var randomIndex=ThreadLocalRandom.current().nextInt(0,knownProposaList.size());
            var proposalDiscussionId =  knownProposaList.get(randomIndex);
           return Collections.singletonMap("proposalDiscussionId", proposalDiscussionId);
       }
        ).iterator();

        return group("Login")
                .on(feed(RandomDataFeeder.stakeKey)
                        .feed(drepStream)
                        .feed(proposalListStream)
                        .exec(ApiService.getCurrentDelegation)
                        .exec(ApiService.getParams)
                        .exec(ApiService.getDRepVotingPower)
                        .exec(ApiService.getAdaHolderVotingPower));
    }
}
