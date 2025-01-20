package org.cardano.govtool.feeders;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class ProposalListFetcher {
    private static final int PAGE_SIZE = 25;

    public static List<String> fetchProposalIds(String baseUrl) {
        baseUrl = baseUrl.endsWith("/") ? baseUrl : baseUrl + "/";
        try {
            HttpClient client = HttpClient.newHttpClient();
            List<String> allIds = new ArrayList<>();
            ObjectMapper mapper = new ObjectMapper();

            // Fetch about 5 pages
            for (int page = 1; page <= 5; page++) {

                for (int i = 1; i <= 2; i++) {
                    String requestUrl = baseUrl + "proposals" + "?filters[$and][0][gov_action_type_id]=" + i
                            + "&filters[$and][1][prop_name][$containsi]=&filters[$and][2][prop_submitted]=false&pagination[page]="
                            + page + "&pagination[pageSize]=" + PAGE_SIZE
                            + "&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals";

                    HttpRequest request = HttpRequest.newBuilder()
                            .uri(new URI(requestUrl))
                            .GET()
                            .build();

                    HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

                    JsonNode rootNode = mapper.readTree(response.body()).get("data");

                    // select the proposalId field
                    for (JsonNode node : rootNode) {
                        if (node.has("id")) {
                            allIds.add(node.get("id").asText());
                        }
                    }

                }
            }
            return allIds;

        } catch (Exception e) {
            throw new RuntimeException("Failed to fetch available proposals");
        }
    }
}
