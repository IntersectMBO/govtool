export type AutomatedVotingCardProps = {
  description: string;
  onClickDelegate: () => void;
  onClickInfo: () => void;
  title: string;
  votingPower: string | number;
};
