export const parseVotingPowerAndPercentage = (
  combinedString: string
): { votingPower: string; percentage: string } => {
  const splitString = combinedString.split("-");
  if (splitString.length !== 2) {
    throw new Error("Invalid format: expected 'votingPower - percentage'");
  }

  const votingPower = splitString[0].trim();
  const percentage = splitString[1].trim();

  return {
    votingPower,
    percentage,
  };
};
