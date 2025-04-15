import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Button } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { DataMissingHeader } from "@molecules";
import { DRepData } from "@/models";

type DRepDetailsProps = {
  dRepData: DRepData;
  isMe?: boolean;
  isMyDrep?: boolean;
  isValidating?: boolean;
  metadataStatus?: MetadataValidationStatus;
};

export const DRepDetailsCardHeader = ({
  dRepData,
  isMe,
  isMyDrep,
  isValidating,
  metadataStatus,
}: DRepDetailsProps) => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { screenWidth } = useScreenDimension();

  const { givenName, image } = dRepData;

  const navigateToEditDRep = () => {
    navigate(PATHS.editDrepMetadata, {
      state: dRepData,
    });
  };

  return (
    <div>
      {(isMe || isMyDrep) && (
        <Box
          sx={{
            alignSelf: "stretch",
            display: "flex",
            alignItems: "center",
            justifyContent: "flex-end",
            mb: "18px",
            ...(screenWidth <= 1020 && {
              flexDirection: "column",
              gap: 3,
            }),
          }}
        >
          {isMe && (
            <Box
              sx={{
                alignItems: "center",
                display: "flex",
                gap: 1,
                ...(screenWidth < 1020 && {
                  width: "100%",
                }),
              }}
            >
              <Button
                data-testid="edit-drep-data-button"
                onClick={navigateToEditDRep}
                variant="outlined"
                sx={{
                  ...(screenWidth < 1020 && {
                    width: "100%",
                  }),
                }}
              >
                {t("dRepDirectory.editBtn")}
                <img
                  alt="sorting active"
                  src={ICONS.editIcon}
                  style={{ marginLeft: "4px" }}
                />
              </Button>
            </Box>
          )}
        </Box>
      )}
      <DataMissingHeader
        isDRep
        title={givenName ?? undefined}
        image={image}
        isDataMissing={metadataStatus}
        titleStyle={{ wordBreak: "break-word", whiteSpace: "wrap" }}
        isValidating={isValidating}
      />
    </div>
  );
};
