import { Link } from "@mui/material";
import { Trans } from "react-i18next";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { correctAdaFormat, openInNewTab } from "@/utils";
import { useCardano } from "@/context";

export const RetireAsSoleVoterBoxContent = () => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { soleVoter } = useCardano();

  return (
    <>
      <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
        {t("soleVoter.retirementHeading")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{
          mb: 7,
          mt: isMobile ? 4 : 10,
          textAlign: "center",
          whiteSpace: "pre-line",
          textDecoration: "underline",
        }}
        variant="body1"
      >
        <Trans
          i18nKey="soleVoter.retirementDescription"
          values={{ deposit: correctAdaFormat(soleVoter?.deposit) }}
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
