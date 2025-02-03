import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import { NodeObject } from "jsonld";

import { downloadJson, generateJsonld, generateMetadataBody } from "@utils";
import { MetadataValidationStatus } from "@models";
import { CIP_100_CONTEXT } from "@/consts";

import { useValidateMutation } from "../mutations";

export type VoteContextFormValues = {
  voteContextText: string;
  terms?: boolean;
  storingURL: string;
};

export const useVoteContextForm = (
  setSavedHash?: Dispatch<SetStateAction<string | null>>,
  setStep?: Dispatch<SetStateAction<number>>,
  setErrorMessage?: Dispatch<SetStateAction<string | undefined>>,
) => {
  const { validateMetadata } = useValidateMutation();
  const [hash, setHash] = useState<string | null>(null);
  const [json, setJson] = useState<NodeObject | null>(null);

  const {
    control,
    formState: { errors, isValid },
    getValues,
    handleSubmit,
    setValue,
    watch,
    register,
    reset,
  } = useFormContext<VoteContextFormValues>();

  const generateMetadata = useCallback(async () => {
    const { voteContextText } = getValues();
    const body = generateMetadataBody({
      data: {
        comment: voteContextText,
      },
      acceptedKeys: ["comment"],
    });
    const jsonld = await generateJsonld(body, CIP_100_CONTEXT);

    const jsonHash = blake2bHex(JSON.stringify(jsonld, null, 2), undefined, 32);

    // That allows to validate metadata hash
    setHash(jsonHash);
    setJson(jsonld);

    return jsonld;
  }, [getValues]);

  const onClickDownloadJson = () => {
    if (!json) return;
    downloadJson(json, "Vote_Context");
  };

  const onSubmit = useCallback(
    async (data: VoteContextFormValues) => {
      try {
        if (!hash) {
          throw new Error(MetadataValidationStatus.INVALID_HASH);
        }

        const result = await validateMetadata({
          hash,
          url: data.storingURL,
        });

        if (result.status) {
          throw result.status;
        }

        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        if (Object.values(MetadataValidationStatus).includes(error)) {
          if (setErrorMessage) setErrorMessage(error);
          if (setStep) setStep(4);
        }
      } finally {
        if (setSavedHash) setSavedHash(hash);
        if (setStep) setStep(4);
      }
    },
    [hash],
  );

  return {
    control,
    validateURL: handleSubmit(onSubmit),
    errors,
    generateMetadata,
    getValues,
    isValid,
    onClickDownloadJson,
    register,
    reset,
    setValue,
    watch,
    hash,
  };
};
