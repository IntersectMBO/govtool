package org.cardano.govtool.feeders;

import org.cardano.govtool.Utils;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class RandomDataFeeder {

    private static <T> Iterator<Map<String, T>> generateIterator(String key, Supplier<T> supplier) {
        return Stream.generate(() -> Collections.singletonMap(key, supplier.get())).iterator();
    }
    
    public static Iterator<Map<String, Object>> proposalId = Stream.generate((Supplier<Map<String, Object>>) () -> {
        String proposalId = Utils.generateTxHash() + "%230";
        return Collections.singletonMap("proposalId", proposalId);
    }).iterator();

    public static Iterator<Map<String, Object>> dRepId = Stream.generate((Supplier<Map<String, Object>>) () -> {
        String dRepKey = Utils.generateDRepIdHash();
        return Collections.singletonMap("dRepId", dRepKey);
    }).iterator();

    public static Iterator<Map<String, Object>> txId = Stream.generate((Supplier<Map<String, Object>>) () -> {
        String txId = Utils.generateTxId();
        return Collections.singletonMap("txId", txId);
    }).iterator();

    public static Iterator<Map<String, Object>> stakeKey = Stream.generate((Supplier<Map<String, Object>>) () -> {
        String stakeKey = Utils.generateStakeKey();
        return Collections.singletonMap("stakeKey", stakeKey);
    }).iterator();

    public static Iterator<Map<String, Object>> commentText = generateIterator("commentText", Utils::generateRandomDescription);
    public static Iterator<Map<String, Object>> proposalVote = generateIterator("proposalVote", Utils::generateRandomBool);

    public static Iterator<Map<String, Object>> title = generateIterator("title", Utils::generateRandomTitle);

    public static Iterator<Map<String, Object>> abstractText = generateIterator("abstract", Utils::generateRandomDescription);

    public static Iterator<Map<String, Object>> motivationText = generateIterator("motivation", Utils::generateRandomDescription);

    public static Iterator<Map<String, Object>> rationaleText = generateIterator("rationale", Utils::generateRandomDescription);

    public static Iterator<Map<String, Object>> proposalAmount = generateIterator("proposalAmount", Utils::generateRandomAmount);

    public static Iterator<Map<String, Object>> governanceActionTypeId = generateIterator("governanceActionTypeId", 
        () -> ThreadLocalRandom.current().nextInt(1, 3));

    public static Iterator<Map<String, Object>> proposalLink = generateIterator("proposalLink", Utils::generateRandomLink);

    public static Iterator<Map<String, Object>> proposalLinkText = generateIterator("proposalLinkText", Utils::generateRandomTitle);

    public static Iterator<Map<String, Object>> proposalReceivingAddress = generateIterator("proposalReceivingAddress", Utils::generateStakeKey);

}
