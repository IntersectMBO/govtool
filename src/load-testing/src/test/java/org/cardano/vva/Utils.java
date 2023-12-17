package org.cardano.vva;

import org.bouncycastle.crypto.digests.Blake2bDigest;
import org.bouncycastle.util.encoders.Hex;

public class Utils {
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
}
