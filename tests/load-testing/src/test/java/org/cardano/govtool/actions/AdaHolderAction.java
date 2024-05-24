package org.cardano.govtool.actions;
import static io.gatling.javaapi.http.HttpDsl.http;

import io.gatling.javaapi.core.ScenarioBuilder;
import org.cardano.govtool.ApiService;
import io.gatling.javaapi.core.ChainBuilder;
import org.cardano.govtool.feeders.RandomDataFeeder;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.status;

public class AdaHolderAction {
    public static ChainBuilder delegateToDRep = group("Delegation").on(exec(ApiService.getAllDReps)
            .pause(6)
            .feed(RandomDataFeeder.txId)
            .exec(ApiService.getTxStatus)
            .pause(4)
            .exec(ApiService.pollTxStatus)
            .feed(RandomDataFeeder.stakeKey)
            .exec(ApiService.getCurrentDelegation));


    /**
     * E
     *
     */
    public static ChainBuilder exploreDrepsChain = exec(http("get_dReps").get("/drep/list?page=0&size=10")
                    .check(jmesPath("elements[].drepId").ofList().transform(dreps ->{
                        Collections.shuffle(dreps); // Shuffle the list to randomize the order
                        int visited_dreps = ThreadLocalRandom.current().nextInt(2, 7);
                        return dreps.subList(0, Math.min(visited_dreps, dreps.size()));
                    }).exists().saveAs("drepIds"))
            )
            .pause(2)
            .foreach("#{drepIds}", "drepId").on(
                    exec( session -> {
                        http("get_dRepDetails").get("/drep/info/#{drepId}")
                                .check(status().is(200));
                        return session;
                        }
                    ).pause(ThreadLocalRandom.current().nextLong(1,4))
            );
}

