import { useEffect, Dispatch, SetStateAction, useState } from "react";
import { Box, Button, CircularProgress, Link, Typography } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import { useMutation } from "react-query";

import { Spacer } from "@atoms";
import { useTranslation } from "@hooks";
import { VoteContextWrapper } from "@organisms";
import { postIpfs } from "@services";
import { downloadTextFile, openInNewTab } from "@utils";
import { NodeObject } from "jsonld";
import { UseFormSetValue } from "react-hook-form";
import { VoteContextFormValues } from "@hooks";
import { LINKS } from "@/consts/links";
import { ICONS } from "@/consts/icons";
import { useSnackbar } from "@context";
import { copyToClipboard } from "@utils";

interface PostIpfsResponse {
  ipfsCid: string;
}

type VoteContextGovToolProps = {
  setStep: Dispatch<SetStateAction<number>>;
  setSavedHash: Dispatch<SetStateAction<string | null>>;
  onCancel: () => void;
  submitVoteContext: () => void;
  jsonldContent: NodeObject | null;
  metadataHash: string | null;
  setValue: UseFormSetValue<VoteContextFormValues>;
};

export const VoteContextGovTool = ({
  setStep,
  setSavedHash,
  onCancel,
  submitVoteContext,
  jsonldContent,
  metadataHash,
  setValue,
}: VoteContextGovToolProps) => {
  const [apiResponse, setApiResponse] = useState<PostIpfsResponse | null>(null);
  const [uploadInitiated, setUploadInitiated] = useState(false); // New state to track upload
  const { t } = useTranslation();
  const { addSuccessAlert } = useSnackbar();

  const openLink = () => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE);

  const { mutate, isLoading } = useMutation<PostIpfsResponse, Error, { content: string }>({
    mutationFn: postIpfs,
    onSuccess: (data) => {
      const ipfsUrl = `ipfs://${data.ipfsCid}`;
      setValue("storingURL", ipfsUrl);
      setSavedHash(metadataHash); // Set savedHash to metadataHash
      setApiResponse(data);
    },
  });

  useEffect(() => {
    if (jsonldContent && !uploadInitiated) {
      mutate({ content: JSON.stringify(jsonldContent, null, 2) });
      setUploadInitiated(true); // Set flag after initiating upload
    }
  }, [jsonldContent, mutate, uploadInitiated]);

  const handleDownload = () => {
    if (jsonldContent) {
      downloadTextFile(JSON.stringify(jsonldContent, null, 2), "voteContext.jsonld");
    }
  };

  return (
    <VoteContextWrapper
      onContinue={submitVoteContext}
      isContinueDisabled={!apiResponse}
      onCancel={onCancel}
    >
      <Typography sx={{ textAlign: "center" }} variant="h4">
        {t("createGovernanceAction.rationalePinnedToIPFS")}
      </Typography>
      <Link
        onClick={openLink}
        sx={{
          cursor: "pointer",
          fontSize: 16,
          fontWeight: 500,
          fontFamily: "Poppins",
          my: 4,
          textAlign: "center",
          textDecoration: "none",
        }}
      >
        {t("createGovernanceAction.readFullGuide")}
        <OpenInNewIcon
          sx={{
            color: "primary",
            height: 17,
            width: 17,
            ml: 0.5,
            verticalAlign: "middle",
          }}
        />
      </Link>
      <Typography sx={{ textAlign: "center" }} variant="h5">
        {t("createGovernanceAction.recommendations")}
      </Typography>
      <Spacer y={4} />
      {isLoading ? (
        <Box sx={{ display: "flex", justifyContent: "center" }}>
          <CircularProgress size={24} />
        </Box>
      ) : apiResponse ? (
        <>
          <Typography fontWeight={400} sx={{ textAlign: "center" }} variant="body1">
            {t("createGovernanceAction.downloadAndStoreMetadataFile")}
          </Typography>
          <Button
            data-testid="metadata-download-button"
            onClick={handleDownload}
            size="large"
            startIcon={<img alt="download" src={ICONS.download} />}
            sx={{ width: "fit-content", alignSelf: "center", my: 4 }}
            variant="outlined"
          >
            {t("govActions.voteContextFileName")}
          </Button>
          <Typography sx={{ textAlign: "center" }} variant="body1">
            {t("createGovernanceAction.rePinYourFile")}
          </Typography>
          <Box sx={{ display: "flex", alignItems: "center", justifyContent: "center", gap: 1 }}>
            <Typography sx={{ textAlign: "center" }} variant="body1">
              {apiResponse.ipfsCid ? `ipfs://${apiResponse.ipfsCid}` : "[URI]"}
            </Typography>
            {apiResponse.ipfsCid && (
              <Link
                onClick={() => {
                  copyToClipboard(`ipfs://${apiResponse.ipfsCid}`);
                  addSuccessAlert(t("alerts.copiedToClipboard"));
                }}
                sx={{ cursor: "pointer", display: "flex", alignItems: "center" }}
              >
                <img alt="copy" src={ICONS.copyBlueIcon} style={{ width: 16, height: 16 }} />
              </Link>
            )}
          </Box>
        </>
      ) : (
        <Typography sx={{ textAlign: "center" }} variant="body1">
          {t("createGovernanceAction.uploadingToIPFS")}
        </Typography>
      )}
    </VoteContextWrapper>
  );
};
