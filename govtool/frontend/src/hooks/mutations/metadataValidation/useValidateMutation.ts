import { useMutation, useQueryClient } from "@tanstack/react-query";

import { postValidate } from "@services";
import { MUTATION_KEYS } from "@consts";
import {
  MetadataValidationDTO,
  MetadataValidationStatus,
  ValidateMetadataResult,
} from "@models";
import { useMemo } from "react";

export const useValidateMutation = <MetadataType>() => {
  const queryClient = useQueryClient();

  const { data, isPending } = useMutation({
    mutationFn: (body: MetadataValidationDTO) =>
      postValidate<MetadataType>(body),
    mutationKey: [MUTATION_KEYS.postValidateKey],
  });

  // A request that fails outright (timeout, network, 5xx) resolves as
  // INTERNAL_ERROR, so every caller handles it like any other status and no
  // validating indicator is left spinning.
  const validateMetadata = async (
    body: MetadataValidationDTO,
  ): Promise<ValidateMetadataResult<MetadataType>> =>
    queryClient
      .fetchQuery({
        queryKey: [
          MUTATION_KEYS.postValidateKey,
          body.hash,
          body.url,
          !!body.verifyUrl,
        ],
        queryFn: () => postValidate<MetadataType>(body),
      })
      .catch((error) => {
        console.error(error);
        return { valid: false, status: MetadataValidationStatus.INTERNAL_ERROR };
      });

  const contextValue = useMemo(
    () => ({
      validateMetadata,
      validationStatus: data,
      isValidating: isPending,
    }),
    [data, isPending],
  );

  return contextValue;
};
