package org.cardano.govtool.simulations;

import io.gatling.javaapi.core.ChainBuilder;
import io.gatling.javaapi.core.PopulationBuilder;
import io.gatling.javaapi.core.Simulation;
import io.gatling.javaapi.http.HttpProtocolBuilder;
import org.cardano.govtool.actions.Action;
import org.cardano.govtool.actions.AdaHolderAction;
import org.cardano.govtool.actions.AuthenticationAction;
import org.cardano.govtool.actions.DRepAction;
import org.cardano.govtool.feeders.DrepListFetcher;
import org.cardano.govtool.feeders.PageVisits;

import java.util.List;
import java.util.Optional;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.http;


public class VvaSimulation extends Simulation {
    private static final String API_URL = Optional.ofNullable(System.getenv("API_URL")).orElse("https://govtool.cardanoapi.io/api2");
    private static final int PEAK_USERS = Integer.parseInt(Optional.ofNullable(System.getenv("PEAK_USERS")).orElse("600"));
    private static final int STRESS_DURATION = Integer.parseInt(Optional.ofNullable(System.getenv("STRESS_DURATION")).orElse("20"));
    private static final int RAMP_DURATION = Integer.parseInt(Optional.ofNullable(System.getenv("RAMP_DURATION")).orElse("20"));
    private List<String>knownDreps;
    @Override
    public void before() {
        System.out.printf("Base API URL: %s%n", API_URL);
        System.out.printf("Peak users count: %d%n", PEAK_USERS);
        System.out.printf("Ramping users over %d seconds%n", RAMP_DURATION);
        System.out.printf("Stress interval %d seconds%n", STRESS_DURATION);

    }

    private PopulationBuilder makeScenario(String name, ChainBuilder chain, double userPercent) {
        var rampUserRate = ((double) PEAK_USERS) * userPercent / (double) RAMP_DURATION;
        return scenario(name).exec(AuthenticationAction.connect(knownDreps).pause(2).exec(chain)).injectOpen(
                nothingFor(5),
                constantUsersPerSec(rampUserRate).during(RAMP_DURATION),
                stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
        );
    }

    private final HttpProtocolBuilder httpProtocol = http
            .baseUrl(API_URL)
            .inferHtmlResources(AllowList(), DenyList(".*\\.js", ".*\\.css", ".*\\.gif", ".*\\.jpeg", ".*\\.jpg", ".*\\.ico", ".*\\.woff", ".*\\.woff2", ".*\\.(t|o)tf", ".*\\.png", ".*\\.svg", ".*detectportal\\.firefox\\.com.*"))
            .acceptHeader("application/json;charset=utf-8")
            .acceptEncodingHeader("gzip, deflate, br")
            .acceptLanguageHeader("en-US,en;q=0.9")
            .userAgentHeader("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36");

    // Load Simulation
    {
        knownDreps= DrepListFetcher.fetchDrepIds(API_URL);
        var DREP_USER_RATI0=0.3;
        setUp(
                  makeScenario("User Connects and Leave", exec(), 0.1)
                , makeScenario("User Registers as Drep",
                        exec(DRepAction.registerAsDRep), 0.1),
                 makeScenario("User Views Proposals",
                        exec(Action.viewProposals), 0.2)
                , makeScenario("AdaHolder delegates to Drep",
                        exec(AdaHolderAction.delegateToDRep), 0.1)
                , makeScenario("ListProposals", PageVisits.visitProposalPage(), 0.2)

                // Further split drep users on scenarios
                , makeScenario("Drep view Votes",
                        exec(Action.viewProposals).pause(2).exec(Action.viewProposals), DREP_USER_RATI0 * 0.5)
                , makeScenario("Drep votes on Proposal",
                        exec(DRepAction.vote), DREP_USER_RATI0 * 0.4)
                , makeScenario("Drep Retirement",
                        exec(DRepAction.retireAsDRep), DREP_USER_RATI0 * 0.1)
        ).protocols(httpProtocol);
    }

}
