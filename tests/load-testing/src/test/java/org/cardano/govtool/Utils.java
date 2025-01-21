package org.cardano.govtool;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import org.bouncycastle.crypto.digests.Blake2bDigest;
import org.bouncycastle.util.encoders.Hex;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.javafaker.Faker;

public class Utils {

    static Faker faker = new Faker();
    private static byte[] blake2b(byte[] data, int digestSize) {
        Blake2bDigest blake2bDigest = new Blake2bDigest(null, digestSize, null, null);
        blake2bDigest.update(data, 0, data.length);
        byte[] hash = new byte[digestSize];
        blake2bDigest.doFinal(hash, 0);
        return hash;
    }

    public static String generateDRepIdHash() {
        byte[] bytes = new byte[28];
        byte[] dRepKey = blake2b(bytes, 28);
        return Hex.toHexString(dRepKey);
    }

    public static String generateTxHash() {
        byte[] bytes = new byte[28];
        byte[] txHash = blake2b(bytes, 28);
        return Hex.toHexString(txHash);
    }

    public static String generateTxId() {
        byte[] bytes = new byte[32];
        byte[] txId = blake2b(bytes, 32);
        return Hex.toHexString(txId);
    }

    public static String generateStakeKey() {
        byte[] bytes = new byte[29];
        byte[] stakeKey = blake2b(bytes, 29);
        return Hex.toHexString(stakeKey);
    }

    public static String generateRandomDescription() {
        return faker.lorem().sentence();
    }

    public static String generateRandomTitle() {
        return faker.name().title();
    }

    public static String generateRandomLink() {
        return faker.internet().url();
    }

    public static String generateRandomAmount() {
        return String.valueOf(faker.number().numberBetween(100, 10000));
    }

    public static boolean generateRandomBool() {
        return faker.bool().bool();
    }

    public static String extractJWT() {
        try {
            String json = Files.readString(Paths.get("src/test/resources/auth.json"));

            ObjectMapper mapper = new ObjectMapper();
            JsonNode rootNode = mapper.readTree(json);
            var randomIndex = ThreadLocalRandom.current().nextInt(0, rootNode.size());
            String jwt = rootNode.get(randomIndex).get("jwt").asText();
            return jwt;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public static String extractBech32StakeAddress() {
        try {
            String json = Files.readString(Paths.get("src/test/resources/auth.json"));

            ObjectMapper mapper = new ObjectMapper();
            JsonNode rootNode = mapper.readTree(json);
            var randomIndex = ThreadLocalRandom.current().nextInt(0, rootNode.size());
            String address = rootNode.get(randomIndex).get("stakeAddress").asText();
            return address;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public static Map<String, String> proposalDefaultHeaders(String jwt) {
        return Map.of(
                "Authorization", "Bearer " + jwt,
                "Content-Type", "application/json");
    }
}
