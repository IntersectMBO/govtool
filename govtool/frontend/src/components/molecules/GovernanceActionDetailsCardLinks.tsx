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
  const { screenWidth } = useScreenDimension();
  const { t } = useTranslation();
  const { openModal } = useModal();

  const isOneLine = screenWidth < 1600;

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
        {links?.map(({ uri, label }) => (
          <Box
            key={`${label}-${uri}-label`}
            display="flex"
            flexDirection="column"
            overflow="hidden"
          >
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
            <Box display="flex" flexDirection="row">
              <Typography
                sx={{
                  fontSize: 16,
                  fontWeight: 400,
                  maxWidth: isOneLine ? "283px" : "auto",
                  lineHeight: "24px",
                  ...(isOneLine && {
                    whiteSpace: "nowrap",
                    overflow: "hidden",
                    textOverflow: "ellipsis",
                  }),
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
          </Box>
        ))}
      </>
    )
  );
};
