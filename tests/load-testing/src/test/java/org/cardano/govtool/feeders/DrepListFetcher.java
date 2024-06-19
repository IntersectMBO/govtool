package org.cardano.govtool.feeders;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class DrepListFetcher {

    private static final int PAGE_SIZE = 20;

    public  static  List<String> fetchDrepIds(String baseUrl) {
        baseUrl = baseUrl.endsWith("/")?baseUrl : baseUrl + "/";
        try {
            HttpClient client = HttpClient.newHttpClient();
            List<String> allIds = new ArrayList<>();
            ObjectMapper mapper = new ObjectMapper();

            // Fetch about 5 pages
            for (int page = 0; page < 5; page++) {
                String requestUrl = baseUrl + "/drep/list" + "?page=" + page + "&size=" + PAGE_SIZE;
                HttpRequest request = HttpRequest.newBuilder()
                        .uri(new URI(requestUrl))
                        .GET()
                        .build();

                HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

                JsonNode rootNode = mapper.readTree(response.body()).get("elements");
                
                // select the drepId field
                List<String> idList = rootNode.findValues("drepId").stream()
                        .map(JsonNode::asText)
                        .toList();
                allIds.addAll(idList);
            }
            return allIds;

        } catch (Exception e) {
            throw new RuntimeException("Failed to fetch available dreps");
        }
    }
}
