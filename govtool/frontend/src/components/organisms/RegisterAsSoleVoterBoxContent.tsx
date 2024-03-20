import { Link } from "@mui/material";
import { Trans } from "react-i18next";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  PROTOCOL_PARAMS_KEY,
  correctAdaFormat,
  getItemFromLocalStorage,
  openInNewTab,
} from "@utils";

export const RegisterAsSoleVoterBoxContent = () => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const epochParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

  return (
    <>
      <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
        {t("soleVoter.registerHeading")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{
          mb: 7,
          mt: isMobile ? 4 : 10,
          textAlign: "center",
          whiteSpace: "pre-line",
        }}
        variant="body1"
      >
        <Trans
          i18nKey="soleVoter.registerDescription"
          values={{ deposit: correctAdaFormat(epochParams.drep_deposit) }}
          components={[
            <Link
              onClick={() => openInNewTab("https://sancho.network/")}
              sx={{ cursor: "pointer" }}
              key="0"
            />,
          ]}
        />
      </Typography>
    </>
  );
};
