import { useEffect, Dispatch, SetStateAction, useState } from "react";
import { Box, Button, CircularProgress, Link, Typography } from "@mui/material";
import { useMutation } from "react-query";

import { VoteContextWrapper } from "@organisms";
import { postIpfs } from "@services";
import { downloadTextFile, openInNewTab } from "@utils";
import { NodeObject } from "jsonld";
import { UseFormSetValue } from "react-hook-form";
import { VoteContextFormValues, useTranslation } from "@hooks";
import { LINKS } from "@/consts/links";
import { ICONS } from "@/consts/icons";
import { primaryBlue } from "@/consts";

interface PostIpfsResponse {
  ipfsCid: string;
}

type VoteContextGovToolProps = {
  setStep: Dispatch<SetStateAction<number>>;
  setSavedHash: Dispatch<SetStateAction<string | null>>;
  jsonldContent: NodeObject | null;
  metadataHash: string | null;
  setValue: UseFormSetValue<VoteContextFormValues>;
};

export const VoteContextGovTool = ({
  setStep,
  setSavedHash,
  jsonldContent,
  metadataHash,
  setValue,
}: VoteContextGovToolProps) => {
  const [apiResponse, setApiResponse] = useState<PostIpfsResponse | null>(null);
  const [uploadInitiated, setUploadInitiated] = useState(false); // New state to track upload
  const { t } = useTranslation();

  const openLink = () => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE);

  const { mutate, isLoading, isError } = useMutation<PostIpfsResponse, Error, { content: string }>({
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
      onCancel={() => { setStep(2); }}
      onContinue={() => { setStep(5); }}
      useBackLabel
      isContinueDisabled={!apiResponse || isError}
      isVoteWithMetadata
    >
      <Typography sx={{ textAlign: "center", fontSize: "28px", fontWeight: 500 }} variant="h4">
        {t("createGovernanceAction.rationalePinnedToIPFS")}
      </Typography>
      <Link
        onClick={openLink}
        sx={{
          cursor: "pointer",
          fontSize: 16,
          fontWeight: 500,
          fontFamily: "Poppins",
          my: 2,
          textAlign: "center",
          textDecoration: "none",
        }}
      >
        {t("createGovernanceAction.learnMore")}
      </Link>
      {isError ? (
        <Typography sx={{ textAlign: "center", color: "error.main", mt: 2 }} variant="body1">
          {t("createGovernanceAction.uploadToIPFSError")}
        </Typography>
      ) : isLoading ? (
        <Box sx={{ display: "flex", justifyContent: "center" }}>
          <CircularProgress size={24} />
        </Box>
      ) : apiResponse ? (
        <>
          <Typography fontWeight={400} sx={{ textAlign: "center", mt: 2 }} variant="body1">
            {t("createGovernanceAction.optionalDownloadAndStoreMetadataFile")}
          </Typography>
          <Button
            data-testid="metadata-download-button"
            onClick={handleDownload}
            size="large"
            startIcon={<img alt="download" src={ICONS.download} />}
            sx={{ width: "fit-content", alignSelf: "center", my: 3, height: "48px", px: 7, fontWeight: 500 }}
            variant="outlined"
          >
            {t("govActions.voteContextFileName")}
          </Button>
          <Typography sx={{ textAlign: "center", my: 1 }} variant="body1">
            {t("createGovernanceAction.rePinYourFileToIPFS")}
          </Typography>
          <Box sx={{ display: "flex", alignItems: "center", justifyContent: "center", gap: 1 }}>
            <Typography
              sx={{ textAlign: "center",
            "&:hover": {
                  textDecoration: "underline",
            }, }}
              variant="body1"
            >
              {apiResponse.ipfsCid ? (
                <a
                  href={`https://ipfs.io/ipfs/${apiResponse.ipfsCid}`}
                  target="_blank"
                  rel="noopener noreferrer"
                  style={{ textDecoration: "none", color: primaryBlue.c500, cursor: "pointer" }}
                >
                  <span style={{ fontWeight: 500 }}>IPFS URI:</span> {`https://ipfs.io/ipfs/${apiResponse.ipfsCid}`}
                </a>
            ) : (
              "[URI]"
            )}
            </Typography>
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
