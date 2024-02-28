import { FC } from "react";
import { AutomatedVotingOptions } from "@organisms";

interface DRepDirectoryContentProps {
  isConnected?: boolean;
}

export const DRepDirectoryContent: FC<DRepDirectoryContentProps> = ({
  isConnected,
}) => <>{isConnected && <AutomatedVotingOptions />}</>;
