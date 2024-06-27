package org.cardano.govtool.feeders;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.gatling.javaapi.core.ChainBuilder;
import io.gatling.javaapi.http.HttpRequestActionBuilder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.http;
import static io.gatling.javaapi.http.HttpDsl.status;

public class PageVisits {
    public static List<String> proposalTypes = Arrays.asList(
            "NoConfidence",
            "NewCommittee",
            "NewConstitution",
            "HardForkInitiation",
            "ParameterChange",
            "TreasuryWithdrawals",
            "InfoAction"
    );
    public static ObjectMapper objectMapper=new ObjectMapper();
    public static HttpRequestActionBuilder listAndSelectDreps(int min, int max){
        return http("get_dReps").get("/drep/list?page=0&size=10")
                .check(jmesPath("elements[].drepId").ofList().transform(dreps ->{
                    Collections.shuffle(dreps);
                    int visited_dreps = ThreadLocalRandom.current().nextInt(min, max);
                    return dreps.subList(0, Math.min(visited_dreps, dreps.size()));
                }).exists().saveAs("drepIds"));
    }


    public static ChainBuilder visitProposalPage(){


        return exec(proposalTypes.stream().map(pType ->exec(requestProposal(pType))
                .doIf(session -> session.get("metadataList"+pType)!=null)
                .then(foreach("#{metadataList" +pType + "}","metadata"+pType)
                .on(exec(
                        http("validate proposal metadata")
                                .post("/metadata/validate")
                                .body(StringBody(session1 -> session1.get("metadata"+pType)
                                )).header("content-type","application/json")
                                .check(status().is(200))
                )))

        ).toList());
    }
    public static HttpRequestActionBuilder requestProposal(String type){
        return http("list proposal type "+type)
                .get("/proposal/list")
                .queryParam("page","0")
                .queryParam("size","7")
                .multivaluedQueryParam("type",List.of(type))
                .check(status().is(200))
                .check(jmesPath("elements[*]").ofList().transform(rawElements -> {

                    List<Map<String, Object>> elements = rawElements.stream().map(v -> (Map<String,Object>) v).toList();

                    List<String> metaDataList = elements.stream().map(proposal -> {
                                var metadataHash = proposal.get("metadataHash").toString();
                                var url = proposal.get("url");
                                var mp= Map.of("url",url,"hash",metadataHash);
                                try {
                                    return objectMapper.writeValueAsString(mp);
                                } catch (JsonProcessingException e) {
                                    throw new RuntimeException(e);
                                }
                            })
                            .collect(Collectors.toList());
                    return metaDataList;
                }).exists().saveAs("metadataList"+type));
    }
}
