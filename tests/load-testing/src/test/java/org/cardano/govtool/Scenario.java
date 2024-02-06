package org.cardano.govtool;

import io.gatling.javaapi.core.ScenarioBuilder;
import org.cardano.govtool.actions.Action;
import org.cardano.govtool.actions.AdaHolderAction;
import org.cardano.govtool.actions.AuthenticationAction;
import org.cardano.govtool.actions.DRepAction;

import static io.gatling.javaapi.core.CoreDsl.scenario;

public class Scenario {
    // User connects and leave
    public static final ScenarioBuilder userConnectAndLeave = scenario("User connect and leave")
            .exec(AuthenticationAction.connect);

    // User register as DRep
    public static final ScenarioBuilder userRegisterAsDRep = scenario("User registers as DRep")
            .exec(AuthenticationAction.connect)
            .pause(2)
            .exec(DRepAction.registerAsDRep);

    // User views proposal
    public static final ScenarioBuilder userOnlyViewsProposal = scenario("User views proposal")
            .exec(AuthenticationAction.connect)
            .pause(2)
            .exec(Action.viewProposals);

    // DRep votes on proposal
    public static final ScenarioBuilder dRepVoteOnProposal = scenario("DRep vote on proposal")
            .exec(AuthenticationAction.connect)
            .pause(2)
            .exec(DRepAction.vote);

    // DRep view votes
    public static final ScenarioBuilder dRepViewVotes = scenario("DRep view votes")
            .exec(AuthenticationAction.connect)
            .pause(2)
            .exec(Action.viewProposals)
            .pause(2)
            .exec(Action.viewVotes);

    // User retire as DRep
    public static final ScenarioBuilder dRepRetires = scenario("DRep retirement")
            .exec(AuthenticationAction.connect)
            .pause(2)
            .exec(DRepAction.retireAsDRep);

    // Ada holder delegate to DRep
    public static final ScenarioBuilder adaHolderDelegateToDRep = scenario("DRep delegation")
            .exec(AuthenticationAction.connect)
            .pause(2)
            .exec(AdaHolderAction.delegateToDRep);
}
