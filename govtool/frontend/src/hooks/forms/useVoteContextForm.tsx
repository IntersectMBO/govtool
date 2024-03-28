import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { NodeObject } from "jsonld";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";

import {
  CIP_108,
  MetadataHashValidationErrors,
  VOTE_TEST_CONTEXT,
} from "@consts";
import {
  canonizeJSON,
  downloadJson,
  generateJsonld,
  validateMetadataHash,
} from "@utils";
import { captureException } from "@sentry/react";

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
    const data = getValues();

    const acceptedKeys = ["voteContextText"];

    const filteredData = Object.entries(data)
      .filter(([key]) => acceptedKeys.includes(key))
      .map(([key, value]) => [CIP_108 + key, value]);

    const body = {
      ...Object.fromEntries(filteredData),
    };

    const jsonld = await generateJsonld(body, VOTE_TEST_CONTEXT);
    const canonizedJson = await canonizeJSON(jsonld);
    const canonizedJsonHash = blake2bHex(canonizedJson, undefined, 32);

    // That allows to validate metadata hash
    setHash(canonizedJsonHash);
    setJson(jsonld);

    return jsonld;
  }, [getValues]);

  const onClickDownloadJson = useCallback(() => {
    if (!json) return;
    downloadJson(json, "Vote_Context");
  }, [json]);

  const validateHash = useCallback(
    async (storingUrl: string, localHash: string | null) => {
      try {
        if (!localHash) {
          throw new Error(MetadataHashValidationErrors.INVALID_HASH);
        }
        await validateMetadataHash(storingUrl, localHash);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        if (
          Object.values(MetadataHashValidationErrors).includes(error.message)
        ) {
          if (setErrorMessage) setErrorMessage(error.message);
          if (setStep) setStep(4);
        }
        throw error;
      }
    },
    [],
  );

  const onSubmit = useCallback(
    async (data: VoteContextFormValues) => {
      try {
        await validateHash(data.storingURL, hash);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        captureException(error);
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
