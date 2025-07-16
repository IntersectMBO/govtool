import environments from "@constants/environments";
import { ProposalType } from "@types";
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
      private: environments.faucet.stake.private || "",
    },
    address: environments.faucet.address || "",
  };
};


export const getProposalType = () => {
  return Object.values(ProposalType).filter(
    (type) =>
      !(
        environments.isHardforkProposalEnabled === false &&
        type === ProposalType.hardFork
      )
  );
};

export const getProposalWalletCount = (): number => {
  if (environments.isScheduled) {
    return 1;
  }
  return environments.isHardforkProposalEnabled ? 6 : 5;
};