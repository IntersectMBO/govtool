import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { LinkWithIcon } from "@molecules";

export const GovernanceActionDetailsCardLinks = ({
  links,
}: {
  links?: string[];
}) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { openModal } = useModal();

  return (
    !!links?.length && (
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
            my: 2,
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
          {links.map((link) => (
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
    )
  );
};
