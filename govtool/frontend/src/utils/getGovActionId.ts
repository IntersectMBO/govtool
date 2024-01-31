export const getShortenedGovActionId = (txHash: string, index: number) => {
  const firstPart = txHash.slice(0, 4);
  const lastPart = txHash.slice(-4);

  return `${firstPart}...${lastPart}#${index}`;
};

export const getFullGovActionId = (txHash: string, index: number) => {
  return `${txHash}#${index}`;
};
