import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";

export const GovernanceActionDetailsCardLinks = ({
  links,
}: {
  links?: Reference[];
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
          {links.map(({ uri, label }) => (
            <Box flexDirection={isMobile ? "column" : "row"} display="flex">
              {label && (
                <Typography
                  data-testid={`${label}-${uri}-label`}
                  sx={{
                    fontWeight: 400,
                    flex: 1,
                    fontSize: 16,
                    lineHeight: "24px",
                    mr: 8,
                    overflow: "hidden",
                    width: "auto",
                    whiteSpace: "nowrap",
                  }}
                >
                  {label}
                </Typography>
              )}
              <Typography
                sx={{
                  fontSize: 16,
                  fontWeight: 400,
                  maxWidth: "283px",
                  lineHeight: "24px",
                  whiteSpace: "nowrap",
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  color: "primaryBlue",
                }}
              >
                {uri}
              </Typography>
              {label && (
                <Box ml={1}>
                  <img
                    data-testid="link-button"
                    alt="link"
                    src={ICONS.externalLinkIcon}
                    style={{ cursor: "pointer" }}
                    onClick={() => {
                      openModal({
                        type: "externalLink",
                        state: {
                          externalLink: uri,
                        },
                      });
                    }}
                  />
                </Box>
              )}
            </Box>
          ))}
        </Box>
      </>
    )
  );
};
