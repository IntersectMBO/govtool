package org.cardano.govtool.simulations;

import io.gatling.javaapi.core.ChainBuilder;
import io.gatling.javaapi.core.PopulationBuilder;
import io.gatling.javaapi.core.Simulation;
import io.gatling.javaapi.http.HttpProtocolBuilder;

import org.cardano.govtool.actions.Action;
import org.cardano.govtool.actions.AdaHolderAction;
import org.cardano.govtool.actions.AuthenticationAction;
import org.cardano.govtool.actions.DRepAction;
import org.cardano.govtool.actions.ProposalAction;
import org.cardano.govtool.feeders.DrepListFetcher;
import org.cardano.govtool.feeders.PageVisits;
import org.cardano.govtool.feeders.ProposalListFetcher;

import java.util.List;
import java.util.Optional;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.http;

public class VvaSimulation extends Simulation {

        private List<String> knownDreps;
        private List<String> knownProposaList;

        @Override
        public void before() {
                System.out.printf("Base API URL: %s%n", SimulationConfig.API_URL);
                System.out.printf("Metadata validation API url: %s%n", SimulationConfig.METADATA_VALIDATION_API_URL);
                System.out.printf("PDF API URL: %s%n", SimulationConfig.PDF_API_URL);
                System.out.printf("Peak users count: %d%n", SimulationConfig.PEAK_USERS);
                System.out.printf("Ramping users over %d seconds%n", SimulationConfig.RAMP_DURATION);
                System.out.printf("Stress interval %d seconds%n", SimulationConfig.STRESS_DURATION);

        }
        @Override
        public void after() {
                this.before();
        }

        private PopulationBuilder makeScenario(String name, ChainBuilder chain, double userPercent,
                        HttpProtocolBuilder protocol) {
                var rampUserRate = ((double) SimulationConfig.PEAK_USERS) * userPercent / (double) SimulationConfig.RAMP_DURATION;
                var baseScenario = scenario(name)
                                .exec(AuthenticationAction.connect(knownDreps, knownProposaList).pause(2));
                var withExec = chain == null? baseScenario: baseScenario.exec(chain);

                return withExec.injectOpen(
                        nothingFor(5),
                        constantUsersPerSec(rampUserRate).during(SimulationConfig.RAMP_DURATION),
                        stressPeakUsers(SimulationConfig.PEAK_USERS).during(SimulationConfig.STRESS_DURATION))
                .protocols(protocol);
        }

        private final HttpProtocolBuilder httpProtocol = http
                        .baseUrl(SimulationConfig.API_URL)
                        .inferHtmlResources(AllowList(),
                                        DenyList(".*\\.js", ".*\\.css", ".*\\.gif", ".*\\.jpeg", ".*\\.jpg", ".*\\.ico",
                                                        ".*\\.woff",
                                                        ".*\\.woff2", ".*\\.(t|o)tf", ".*\\.png", ".*\\.svg",
                                                        ".*detectportal\\.firefox\\.com.*"))
                        .acceptHeader("application/json;charset=utf-8")
                        .acceptEncodingHeader("gzip, deflate, br")
                        .acceptLanguageHeader("en-US,en;q=0.9")
                        .userAgentHeader(
                                        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36");

        private final HttpProtocolBuilder pdfHttpProtocol = http
                        .baseUrl(SimulationConfig.PDF_API_URL)
                        .inferHtmlResources(AllowList(),
                                        DenyList(".*\\.js", ".*\\.css", ".*\\.gif", ".*\\.jpeg", ".*\\.jpg", ".*\\.ico",
                                                        ".*\\.woff",
                                                        ".*\\.woff2", ".*\\.(t|o)tf", ".*\\.png", ".*\\.svg",
                                                        ".*detectportal\\.firefox\\.com.*"))
                        .acceptHeader("application/json;charset=utf-8")
                        .acceptEncodingHeader("gzip, deflate, br")
                        .acceptLanguageHeader("en-US,en;q=0.9")
                        .userAgentHeader(
                                        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36");

        // Load Simulation
        {
                knownDreps = DrepListFetcher.fetchDrepIds(SimulationConfig.API_URL);
                knownProposaList = ProposalListFetcher.fetchProposalIds(SimulationConfig.PDF_API_URL);

                var DREP_USER_RATI0 = 0.3;
                setUp(
                                makeScenario("User Connects and Leave", null, 0.1, httpProtocol),
                                makeScenario("User Registers as Drep",
                                                exec(DRepAction.registerAsDRep), 0.1, httpProtocol),
                                makeScenario("User Views Proposals",
                                                exec(Action.viewProposals), 0.2, httpProtocol),
                                makeScenario("AdaHolder delegates to Drep",
                                                exec(AdaHolderAction.delegateToDRep), 0.1, httpProtocol),
                                makeScenario("ListProposals", PageVisits.visitProposalPage(),
                                                0.2, httpProtocol)

                                // Further split drep users on scenarios
                                , makeScenario("Drep view Votes",
                                                exec(Action.viewProposals).pause(2).exec(Action.viewProposals),
                                                DREP_USER_RATI0 * 0.5, httpProtocol),
                                makeScenario("Drep votes on Proposal",
                                                exec(DRepAction.vote), DREP_USER_RATI0 * 0.4, httpProtocol),
                                makeScenario("Drep Retirement",
                                                exec(DRepAction.retireAsDRep), DREP_USER_RATI0 * 0.1, httpProtocol),

                                makeScenario("List Proposal Discussions", PageVisits.visitProposalDiscussionPage(), 0.1,
                                                pdfHttpProtocol),
                                makeScenario("Users Comment on proposal", exec(ProposalAction.commentOnProposal), 0.2,
                                                pdfHttpProtocol),
                                makeScenario("Proposal Active/InActive polls status", exec(ProposalAction.getPolls),
                                                0.2,
                                                pdfHttpProtocol),
                                makeScenario("List Proposal types", exec(ProposalAction.getGovernanceActionType), 0.1,
                                                pdfHttpProtocol),
                                makeScenario("Users Like/Dislike proposal", exec(ProposalAction.voteOnProposal), 0.2,
                                                pdfHttpProtocol),
                                makeScenario("Users Create proposal/draft", exec(ProposalAction.createProposal), 0.2,
                                                pdfHttpProtocol)
                );
        }

}
