import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import { captureException } from "@sentry/react";

import { downloadTextFile } from "@utils";
import { MetadataValidationStatus } from "@models";

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

    const canonizedJsonHash = blake2bHex(voteContextText, undefined, 32);

    // That allows to validate metadata hash
    setHash(canonizedJsonHash);

    return voteContextText;
  }, [getValues]);

  const onClickDownloadFile = useCallback(() => {
    const { voteContextText } = getValues();
    if (!voteContextText) return;
    downloadTextFile(voteContextText, "Vote_Context");
  }, [getValues]);

  const validateHash = useCallback(
    async (url: string, localHash: string | null) => {
      try {
        if (!localHash) {
          throw new Error(MetadataValidationStatus.INVALID_HASH);
        }

        const result = await validateMetadata({
          hash: localHash,
          url,
          noStandard: true,
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
    onClickDownloadFile,
    register,
    reset,
    setValue,
    watch,
    hash,
  };
};
