import { FC } from "react";
import { AutomatedVotingOptions } from ".";

interface DRepDirectoryContentProps {
  isConnected?: boolean;
}

export const DRepDirectoryContent: FC<DRepDirectoryContentProps> = ({
  isConnected,
}) => {
  return <>{isConnected && <AutomatedVotingOptions />}</>;
};
