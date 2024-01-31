package org.cardano.vva.feeders;

import org.cardano.vva.Utils;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class RandomDataFeeder {
    public static Iterator<Map<String, Object>> proposalId =
            Stream.generate((Supplier<Map<String, Object>>) () -> {
                        String proposalId = Utils.generateTxHash() + "%230";
                        return Collections.singletonMap("proposalId", proposalId);
                    }
            ).iterator();

    public static Iterator<Map<String, Object>> dRepId =
            Stream.generate((Supplier<Map<String, Object>>) () -> {
                        String dRepKey = Utils.generateDRepIdHash();
                        return Collections.singletonMap("dRepId", dRepKey);
                    }
            ).iterator();

    public static Iterator<Map<String, Object>> txId =
            Stream.generate((Supplier<Map<String, Object>>) () -> {
                String txId = Utils.generateTxId();
                return Collections.singletonMap("txId", txId);
            }).iterator();

    public static Iterator<Map<String, Object>> stakeKey =
            Stream.generate((Supplier<Map<String, Object>>) () -> {
                String stakeKey = Utils.generateStakeKey();
                return Collections.singletonMap("stakeKey", stakeKey);
            }).iterator();
}
