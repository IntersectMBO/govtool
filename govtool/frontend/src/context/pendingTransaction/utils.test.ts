import { QueryClient } from "@tanstack/react-query";

import { QUERY_KEYS } from "@/consts";
import { getQueryKey, refetchData } from "./utils";

describe("refetchData", () => {
  it("reads voter info cached under the pending transaction's key and a DRep ID", async () => {
    const client = new QueryClient();
    const transaction = {
      type: "registerAsDrep",
      transactionHash: "ab",
      time: new Date().toISOString(),
    } as const;
    client.setQueryData([QUERY_KEYS.useGetDRepInfoKey, "ab", "aa"], {
      isRegisteredAsDRep: true,
    });

    const result = await refetchData(
      "registerAsDrep",
      client,
      getQueryKey("registerAsDrep", transaction),
      undefined,
    );

    expect(result).toBe(true);
  });
});
