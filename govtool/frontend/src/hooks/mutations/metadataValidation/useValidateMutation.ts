import { useMutation, useQueryClient } from "react-query";

import { postValidate } from "@services";
import { MUTATION_KEYS } from "@consts";
import { MetadataValidationDTO } from "@models";
import { useMemo } from "react";

export const useValidateMutation = <MetadataType>() => {
  const queryClient = useQueryClient();

  const { data, isLoading } = useMutation({
    mutationFn: (body: MetadataValidationDTO) =>
      postValidate<MetadataType>(body),
    mutationKey: [MUTATION_KEYS.postValidateKey],
  });

  const validateMetadata = async (body: MetadataValidationDTO) =>
    queryClient.fetchQuery({
      queryKey: [MUTATION_KEYS.postValidateKey, body.hash, body.url],
      queryFn: () => postValidate<MetadataType>(body),
    });

  const contextValue = useMemo(
    () => ({
      validateMetadata,
      validationStatus: data,
      isValidating: isLoading,
    }),
    [data, isLoading],
  );

  return contextValue;
};
