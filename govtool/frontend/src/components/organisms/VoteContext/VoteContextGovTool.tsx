import { useEffect, Dispatch, SetStateAction, useState } from "react";
import { Box, Button, CircularProgress, Link, Typography } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import { useMutation } from "react-query";

import { Spacer } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
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
import { primaryBlue } from "@/consts";

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

  const { isMobile } = useScreenDimension();

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
      showAllButtons={false}
    >
      <Typography sx={{ textAlign: "center" , fontSize : "28px" , fontWeight: 500 }} variant="h4">
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
      {isLoading ? (
        <Box sx={{ display: "flex", justifyContent: "center" }}>
          <CircularProgress size={24} />
        </Box>
      ) : apiResponse ? (
        <>
          <Typography fontWeight={400} sx={{ textAlign: "center" , mt: 2 }} variant="body1">
            {t("createGovernanceAction.optionalDownloadAndStoreMetadataFile")}
          </Typography>
          <Button
            data-testid="metadata-download-button"
            onClick={handleDownload}
            size="large"
            startIcon={<img alt="download" src={ICONS.download} />}
            sx={{ width: "fit-content", alignSelf: "center" , my:3 , height : "48px", px : 7 , fontWeight : 500 }}
            variant="outlined"
          >
            {t("govActions.voteContextFileName")}
          </Button>
          <Typography sx={{ textAlign: "center" , my:1 }} variant="body1">
            {t("createGovernanceAction.rePinYourFileToIPFS")}
          </Typography>
          <Box sx={{ display: "flex", alignItems: "center", justifyContent: "center", gap: 1 }}>
         <Typography sx={{ textAlign: "center" ,
            "&:hover": {
                  textDecoration: "underline",
            }, }} variant="body1">
            {apiResponse.ipfsCid ? (
              <a
                href={`https://ipfs.io/ipfs/${apiResponse.ipfsCid}`}
                target="_blank"
                rel="noopener noreferrer"
                style={{ textDecoration: "none", color: primaryBlue.c500, cursor: "pointer" }}
              >
                <span style={{fontWeight : 500}}>IPFS URI:</span> {`https://ipfs.io/ipfs/${apiResponse.ipfsCid}`}
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
        <Box sx={{ display: "flex", flexDirection: isMobile ? "column" : "row" ,  width: "100%", justifyContent: isMobile ? "none" : "space-between" , gap: isMobile ? "14px" : "0px" , mt: 3 }}>
                <Button
                  variant="outlined"
                  onClick={()=>{setStep(2)}}
                  sx={{ width: isMobile ? "100%" : "96px", whiteSpace: "nowrap" , height:"48px" , fontWeight:"500" }}
                >
                  {t("createGovernanceAction.back")}
                </Button>
                <Button
                  variant="contained"
                  onClick={submitVoteContext}
                  sx={{ width: isMobile ? "100%" :  "130px", whiteSpace: "nowrap" , height:"48px" , fontWeight:"500" }}
                >
                  {t("createGovernanceAction.continue")}
                </Button>
              </Box>
    </VoteContextWrapper>
  );
};
