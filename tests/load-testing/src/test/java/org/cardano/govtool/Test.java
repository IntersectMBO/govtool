package org.cardano.govtool;

public class Test {
    public static void main(String[] args) {
        String dRepIdHash = Utils.generateDRepIdHash();
        String txId = Utils.generateTxId();
        String txHash = Utils.generateTxHash();
        String stakeKey = Utils.generateStakeKey();

        System.out.printf("DRep id hash: %s, length: %d%n", dRepIdHash, dRepIdHash.length());
        System.out.printf("Tx id: %s, length: %d%n", txId, txId.length());
        System.out.printf("Tx hash: %s, length: %d%n", txHash, txHash.length());
        System.out.printf("Stake Key: %s, length: %d%n", stakeKey, stakeKey.length());
    }

}
