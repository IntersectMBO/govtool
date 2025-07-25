import { useEffect, Dispatch, SetStateAction, useState } from "react";
import { Box, Button, CircularProgress, Link, Typography } from "@mui/material";
import { useMutation } from "react-query";

import { Spacer } from "@atoms";
import { useTranslation } from "@hooks";
import { VoteContextWrapper } from "@organisms";
import { postIpfs } from "@services";
import { downloadTextFile } from "@utils";
import { NodeObject } from "jsonld";
import { UseFormSetValue } from "react-hook-form";
import { VoteContextFormValues } from "@hooks";

interface PostIpfsResponse {
  ipfsHash: string;
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
  const [apiResponse, setApiResponse] = useState<string | null>(null);
  const { t } = useTranslation();

  const { mutate, isLoading } = useMutation<PostIpfsResponse, Error, { content: string }>({
    mutationFn: postIpfs,
    onSuccess: (data) => {
      const ipfsUrl = `ipfs://${data.ipfsHash}`;
      setValue("storingURL", ipfsUrl);
      setSavedHash(metadataHash); // Set savedHash to metadataHash
      setApiResponse(JSON.stringify(data, null, 2));
    },
  });

  useEffect(() => {
    if (jsonldContent) {
      mutate({ content: JSON.stringify(jsonldContent, null, 2) });
    }
  }, [jsonldContent, mutate]);

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
        {t("createGovernanceAction.letGovToolStore")}
      </Typography>
      <Spacer y={4} />
      {isLoading ? (
        <Box sx={{ display: "flex", justifyContent: "center" }}>
          <CircularProgress size={24} />
        </Box>
      ) : apiResponse ? (
        <>
          <Typography sx={{ whiteSpace: "pre-wrap", textAlign: "left" }}>
            {apiResponse}
          </Typography>
          <Spacer y={2} />
          <Link onClick={handleDownload} sx={{ cursor: "pointer" }}>
            {t("createGovernanceAction.downloadJsonLd")}
          </Link>
        </>
      ) : (
        <Typography sx={{ textAlign: "center" }} variant="body1">
          {t("createGovernanceAction.uploadingToIPFS")}
        </Typography>
      )}
    </VoteContextWrapper>
  );
};
