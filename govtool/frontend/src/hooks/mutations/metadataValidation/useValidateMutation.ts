import { useMutation } from "react-query";

import { postValidate } from "@services";
import { MUTATION_KEYS } from "@consts";
import { MetadataValidationDTO } from "@models";

export const useValidateMutation = <MetadataType>() => {
  const { data, isLoading, mutateAsync } = useMutation({
    mutationFn: (body: MetadataValidationDTO) => postValidate<MetadataType>(body),
    mutationKey: [MUTATION_KEYS.postValidateKey],
  });

  return {
    validateMetadata: mutateAsync,
    validationStatus: data,
    isValidating: isLoading,
  };
};
