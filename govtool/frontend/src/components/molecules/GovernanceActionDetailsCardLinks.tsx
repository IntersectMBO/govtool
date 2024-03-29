import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { LinkWithIcon } from "@molecules";
import { ICONS } from "@/consts";
import { useModal } from "@/context";

// TODO: When BE is ready, pass links as props
const LINKS = [
  "https://docs.sanchogov.tools/support/get-help-in-discord",
  "https://docs.sanchogov.tools/how-to-use-the-govtool/prerequsites",
  "https://docs.sanchogov.tools/faqs",
  "https://docs.sanchogov.tools/",
];

export const GovernanceActionDetailsCardLinks = () => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { openModal } = useModal();

  return (
    <>
      <Typography
        sx={{
          fontSize: 14,
          fontWeight: 600,
          lineHeight: "20px",
          color: "neutralGray",
          overflow: "hidden",
          textOverflow: "ellipsis",
          whiteSpace: "nowrap",
          mb: 2,
        }}
        data-testid="supporting-links"
      >
        {t("govActions.supportingLinks")}
      </Typography>
      <Box
        sx={{
          display: "grid",
          gridTemplateColumns: isMobile ? undefined : "1fr 1fr",
          columnGap: 2,
          rowGap: 2,
        }}
      >
        {LINKS.map((link) => (
          <LinkWithIcon
            key={link}
            label={link}
            onClick={() => {
              openModal({
                type: "externalLink",
                state: {
                  externalLink: link,
                },
              });
            }}
            icon={<img alt="link" src={ICONS.link} />}
            cutWithEllipsis
          />
        ))}
      </Box>
    </>
  );
};
