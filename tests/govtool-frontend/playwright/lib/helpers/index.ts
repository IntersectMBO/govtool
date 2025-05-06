import environments from "@constants/environments";
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

export const getWalletConfigForFaucet = () => {
  return {
    payment: {
      private: environments.faucet.payment.private || "",
    },
    stake: {
      pkh: environments.faucet.stake.pkh || "",
    },
    address: environments.faucet.address || "",
  };
};
