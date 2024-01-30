import { useMemo } from "react";

import { ICONS } from "@consts";
import { useSnackbar } from "@context";
import { useTranslation } from "react-i18next";

interface Props {
  isChecked?: boolean;
  text: string;
  variant?: string;
}

export const CopyButton = ({ isChecked, text, variant }: Props) => {
  const { addSuccessAlert } = useSnackbar();
  const { t } = useTranslation();

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
        addSuccessAlert(t("alerts.copiedToClipboard"));
        e.stopPropagation();
      }}
      src={iconSrc}
      style={{ cursor: "pointer" }}
    />
  );
};
