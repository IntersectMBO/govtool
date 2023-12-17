import { styled } from "@mui/material/styles";

import { ICONS } from "@consts";
import type { SnackbarSeverity } from "@models";

interface Props {
  message: string;
  onClose?: (_event: React.SyntheticEvent | Event, reason?: string) => void;
  severity: SnackbarSeverity;
}

export function SnackbarMessage({ message, severity, onClose }: Props) {
  return (
    <SnackContainer>
      {severity === "success" ? (
        <img
          src={ICONS.checkCircleIcon}
          style={{ height: "18px", width: "18px" }}
        />
      ) : (
        <img
          src={ICONS.warningIcon}
          style={{ height: "18px", width: "18px" }}
        />
      )}
      <div style={{ width: "373px", textAlign: "left" }}>{message}</div>
      {onClose && (
        <img
          alt="close icon"
          onClick={onClose}
          src={ICONS.closeWhiteIcon}
          style={{ cursor: "pointer" }}
        />
      )}
    </SnackContainer>
  );
}

const SnackContainer = styled("span")`
  align-items: center;
  display: flex;
  gap: 8px;
  justify-content: space-between;
  color: white;
  font-weight: 500;
  font-size: 14px;
  line-height: 24px;
  width: 100%;
`;
