package org.cardano.vva;


import io.gatling.javaapi.core.ChainBuilder;
import io.gatling.javaapi.http.HttpRequestActionBuilder;

import static io.gatling.javaapi.core.CoreDsl.*;

import static io.gatling.javaapi.http.HttpDsl.http;
import static io.gatling.javaapi.http.internal.HttpCheckBuilders.status;

public class ApiService {

    public static ChainBuilder getCurrentDelegation = exec(http("get_current_delegation").get("/ada-holder/get-current-delegation/#{stakeKey}"));

    public static HttpRequestActionBuilder getAdaHolderVotingPower = http("get_AdaHolder_voting_power").get("/ada-holder/get-voting-power/#{stakeKey}");

    public static HttpRequestActionBuilder getDRepVotingPower = http("get_DRep_voting_power").get("/drep/get-voting-power/#{dRepId}");

    public static ChainBuilder getVotes = exec(http("get_votes").get("/drep/getVotes/#{dRepId}"));

    public static ChainBuilder getAllDReps = exec(http("get_dReps").get("/drep/list"));

    public static ChainBuilder getParams = exec(http("get_epoch_params").get("/epoch/params"));

    public static ChainBuilder getProposal = exec(http("get_proposal_detail").get("/proposal/get/#{proposalId}").check(status().is(404)));

    public static ChainBuilder getAllProposals = exec(http("get_proposals").get("/proposal/list"));

    public static ChainBuilder getTxStatus = exec(http("get_tx_status").get("/transaction/status/#{txId}"));

    //TODO randomize the number of repeats, Range 2 to 6
    public static ChainBuilder pollTxStatus = repeat(5).on(
            exec(http("get_tx_status").get("/transaction/status/#{txId}")).pause(4));
}
