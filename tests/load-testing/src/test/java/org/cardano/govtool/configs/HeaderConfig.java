package org.cardano.govtool.configs;

import java.util.Map;

public class HeaderConfig {
    public static Map<CharSequence, String> postHeader = Map.ofEntries(
            Map.entry("content-type", "application/json;charset=utf-8")
    );
}
