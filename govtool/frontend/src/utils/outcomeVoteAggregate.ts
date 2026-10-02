import type { OutcomeVoteAggregate } from "@/models";

/** Keep ledger comparisons exact; round only the percentages displayed in the UI. */
export const outcomeVoteResult = (aggregate: OutcomeVoteAggregate) => {
  const amounts = [
    aggregate.yes,
    aggregate.no,
    aggregate.abstain,
    aggregate.notVoted,
    aggregate.totalEligible,
  ];
  const pattern =
    aggregate.representation === "percent" ? /^\d+(?:\.\d+)?$/ : /^\d+$/;
  const { numerator, denominator } = aggregate.threshold;
  if (
    !amounts.every((value) => pattern.test(value)) ||
    !Number.isSafeInteger(numerator) ||
    !Number.isSafeInteger(denominator) ||
    numerator < 0 ||
    denominator <= 0 ||
    numerator > denominator
  )
    return undefined;

  const digits = Math.max(
    ...amounts.map((value) => value.split(".")[1]?.length ?? 0),
  );
  const scaled = amounts.map((value) => {
    const [whole, fraction = ""] = value.split(".");
    return BigInt(whole + fraction.padEnd(digits, "0"));
  });
  const [yes, no, abstain, notVoted, total] = scaled;
  if (
    yes + no + abstain + notVoted !== total ||
    (aggregate.representation === "percent" && total > 10n ** BigInt(digits))
  )
    return undefined;
  const ratification = total - abstain;
  const decimal = (value: bigint) => {
    if (!digits) return value.toString();
    const padded = value.toString().padStart(digits + 1, "0");
    return `${padded.slice(0, -digits)}.${padded.slice(-digits)}`;
  };
  // Round to hundredths of a percent for display; this never determines passing.
  const yesPercentage =
    ratification > 0n
      ? Number((yes * 10000n + ratification / 2n) / ratification) / 100
      : undefined;
  return {
    ratification: decimal(ratification),
    totalNo: decimal(no + notVoted),
    yesPercentage,
    noPercentage: yesPercentage === undefined ? undefined : 100 - yesPercentage,
    // The ledger's ratio is 0 when nothing non-abstaining remains, so only a
    // zero threshold accepts it.
    passing:
      aggregate.passing ??
      (ratification > 0n
        ? yes * BigInt(denominator) >= ratification * BigInt(numerator)
        : numerator === 0),
  };
};

export const formatOutcomeAggregateValue = (
  value: string,
  representation: OutcomeVoteAggregate["representation"],
) => {
  if (representation === "percent")
    return `${(Number(value) * 100).toFixed(2)}%`;
  const amount = BigInt(value);
  if (representation === "count") return amount.toLocaleString();
  // Whole ADA, rounded up, without losing precision in large lovelace amounts.
  return `₳ ${((amount + 999999n) / 1000000n).toLocaleString()}`;
};
