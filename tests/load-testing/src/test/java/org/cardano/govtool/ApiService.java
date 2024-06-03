package org.cardano.govtool;


import io.gatling.javaapi.core.ChainBuilder;
import io.gatling.javaapi.http.HttpRequestActionBuilder;

import java.util.concurrent.ThreadLocalRandom;

import static io.gatling.javaapi.core.CoreDsl.*;

import static io.gatling.javaapi.http.HttpDsl.http;

public class ApiService {
    static String example_metadata_validate="{\"url\":\"https://metadata-govtool.cardanoapi.io/data/milanfile\",\"hash\":\"dcc25ca74534a1fa3b4c86447c1e28dd86e103e1ead5e5308b64b34338af00a9\"}";
    public static ChainBuilder validate_sample_metadata= exec(http("validate_metadata").post("/metadata/validate").body(StringBody(example_metadata_validate)).header("content-type","application/json"));
    public static ChainBuilder getCurrentDelegation = exec(http("get_current_delegation").get("/ada-holder/get-current-delegation/#{stakeKey}"));

    public static HttpRequestActionBuilder getAdaHolderVotingPower = http("get_AdaHolder_voting_power").get("/ada-holder/get-voting-power/#{stakeKey}");

    public static HttpRequestActionBuilder getDRepVotingPower = http("get_dRep_voting_power").get("/drep/get-voting-power/#{dRepId}");

    public static  HttpRequestActionBuilder getDrepInfo = http("get_dRep_info").get("/drep/info/#{stakeKey}");
    public static ChainBuilder getVotes = exec(http("get_votes").get("/drep/getVotes/#{dRepId}"));

    public static ChainBuilder getAllDReps = exec(http("get_dReps").get("/drep/list?page=0&size=10"));

    public static ChainBuilder getParams = exec(http("get_epoch_params").get("/epoch/params"));

    public static ChainBuilder getTxStatus = exec(http("get_tx_status").get("/transaction/status/#{txId}"));

    public static ChainBuilder getMetrics = exec(http("get_metrics").get("/network/metrics"));

    public static ChainBuilder pollTxStatus = repeat(session -> ThreadLocalRandom.current()
            .nextInt(3,6)).on(
                exec(http("get_tx_status").get("/transaction/status/#{txId}")).pause(4));
}
