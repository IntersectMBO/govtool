import { FC } from "react";

interface DRepDirectoryContentProps {
  isConnected?: boolean;
}

export const DRepDirectoryContent: FC<DRepDirectoryContentProps> = ({
  isConnected,
}) => {
  return (
    <>
      <p>DRepDirectory</p>
      <p>connected: {String(!!isConnected)}</p>
    </>
  );
};
