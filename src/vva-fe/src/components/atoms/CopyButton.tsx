import { useMemo } from "react";

import { ICONS } from "@consts";
import { useSnackbar } from "@context";

interface Props {
  isChecked?: boolean;
  text: string;
  variant?: string;
}

export const CopyButton = ({ isChecked, text, variant }: Props) => {
  const { addSuccessAlert } = useSnackbar();
  const iconSrc = useMemo(() => {
    if (variant === "blue") {
      return ICONS.copyBlueIcon;
    }

    if (isChecked) {
      return ICONS.copyWhiteIcon;
    }

    return ICONS.copyIcon;
  }, [isChecked, variant]);

  return (
    <img
      data-testid={"copy-button"}
      alt="copy"
      onClick={(e) => {
        navigator.clipboard.writeText(text);
        addSuccessAlert("Copied to clipboard.");
        e.stopPropagation();
      }}
      src={iconSrc}
      style={{ cursor: "pointer" }}
    />
  );
};
