import { waitForTxConfirmation } from "@helpers/transaction";

export function withTxConfirmation(value, { kind }) {
  if (kind !== "method") return;

  return async function (...args: any) {
    await waitForTxConfirmation(
      this.page,
      async () => await value.apply(this, args),
    );
  };
}
