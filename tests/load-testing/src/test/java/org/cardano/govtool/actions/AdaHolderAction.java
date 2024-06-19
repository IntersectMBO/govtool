package org.cardano.govtool.actions;
import static io.gatling.javaapi.http.HttpDsl.http;

import org.cardano.govtool.ApiService;
import io.gatling.javaapi.core.ChainBuilder;
import org.cardano.govtool.feeders.RandomDataFeeder;

import java.util.Collections;
import java.util.concurrent.ThreadLocalRandom;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.status;
import static org.cardano.govtool.feeders.PageVisits.listAndSelectDreps;

public class AdaHolderAction {


    public static ChainBuilder delegateToDRep = group("Delegation").on(
                feed(RandomDataFeeder.stakeKey)
            )
            .exec(adaHolderBasicApis())
            .exec( AdaHolderAction.exploreDrepsChain().exec(adaHolderBasicApis()))
            .pause(6)

            .feed(RandomDataFeeder.txId)
            .repeat(6).on(ApiService.getTxStatus.pause(4))
            .exec(adaHolderBasicApis());

    public static  ChainBuilder adaHolderBasicApis(){
        return   exec(ApiService.getDrepInfo)
                .exec(ApiService.getAdaHolderVotingPower)
                .exec(ApiService.getMetrics)
                .exec(ApiService.getCurrentDelegation);
    }


    /**
     *
     */
    public static ChainBuilder exploreDrepsChain () {
    return exec(adaHolderBasicApis().exec(listAndSelectDreps(2,5)))
            .pause(2).exitHereIf(session -> {
                return session.get("drepIds") == null;
            })
            .foreach("#{drepIds}", "drepId").on(
                    exec(
                            http("get_dRepDetails").get("/drep/info/#{drepId}")
                                    .check(status().is(200))
                    ).exec(adaHolderBasicApis().exec(ApiService.getAllDReps))
                            .pause(ThreadLocalRandom.current().nextLong(1,4))
            );

    }
}

