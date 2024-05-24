package org.cardano.govtool.simulations;

import org.cardano.govtool.Scenario;
import io.gatling.javaapi.core.Simulation;
import io.gatling.javaapi.http.HttpProtocolBuilder;

import java.util.Optional;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.http;
import static org.cardano.govtool.actions.AdaHolderAction.drepListDelegateScenario;


public class VvaSimulation extends Simulation {
    private static final String API_URL = Optional.ofNullable(System.getenv("API_URL")).orElse("https://govtool.cardanoapi.io/api");
    private static final int TARGET_USER_RATE = Integer.parseInt(Optional.ofNullable(System.getenv("TARGET_USER_RATE")).orElse("5"));
    private static final int PEAK_USERS = Integer.parseInt(Optional.ofNullable(System.getenv("PEAK_USERS")).orElse("10"));
    private static final int STRESS_DURATION = Integer.parseInt(Optional.ofNullable(System.getenv("STRESS_DURATION")).orElse("10"));
    private static final int RAMP_DURATION = Integer.parseInt(Optional.ofNullable(System.getenv("RAMP_DURATION")).orElse("10"));

    @Override
    public void before() {
        System.out.printf("Base API URL: %s%n", API_URL);
        System.out.printf("Target user rate: %d users/sec%n", TARGET_USER_RATE);
        System.out.printf("Ramping users over %d seconds%n", RAMP_DURATION);
        System.out.printf("Peak users count: %d%n", PEAK_USERS);
        System.out.printf("Stress interval %d seconds%n", STRESS_DURATION);
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
        setUp(
//                Scenario.userConnectAndLeave.injectOpen(
//                        nothingFor(5),
//                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.2).during(RAMP_DURATION),
//                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
//                )

                drepListDelegateScenario.injectOpen(
                        nothingFor(5),
                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.3).during(RAMP_DURATION),
                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
                )
//                Scenario.userRegisterAsDRep.injectOpen(
//                        nothingFor(5),
//                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.3).during(RAMP_DURATION),
//                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
//                ),
//                Scenario.userOnlyViewsProposal.injectOpen(
//                        nothingFor(5),
//                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.1).during(RAMP_DURATION),
//                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
//                ),
//                Scenario.adaHolderDelegateToDRep.injectOpen(
//                        nothingFor(5),
//                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.4).during(RAMP_DURATION),
//                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
//                ),
//                // Further DRep scenarios
//                Scenario.dRepVoteOnProposal.injectOpen(
//                        nothingFor(5),
//                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.3 * 0.4).during(RAMP_DURATION),
//                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
//                ),
//                Scenario.dRepViewVotes.injectOpen(
//                        nothingFor(5),
//                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.3 * 0.5).during(RAMP_DURATION),
//                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
//                ),
//                Scenario.dRepRetires.injectOpen(
//                        nothingFor(5),
//                        rampUsersPerSec(1).to(TARGET_USER_RATE * 0.3 * 0.1).during(RAMP_DURATION),
//                        stressPeakUsers(PEAK_USERS).during(STRESS_DURATION)
//                )
        ).protocols(httpProtocol);
    }

}
