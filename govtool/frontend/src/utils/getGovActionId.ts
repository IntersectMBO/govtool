export const getShortenedGovActionId = (
  txHash: string,
  index: number | string,
) => {
  if (txHash.length <= 6) {
    return `${txHash}#${index}`;
  }

  const firstPart = txHash.slice(0, 4);
  const lastPart = txHash.slice(-4);

  return `${firstPart}...${lastPart}#${index}`;
};

export const getFullGovActionId = (txHash: string, index: number | string) =>
  `${txHash}#${index}`;
