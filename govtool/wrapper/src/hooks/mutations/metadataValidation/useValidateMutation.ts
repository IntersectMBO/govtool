import { useMutation } from "react-query";

import { postValidate } from "@services";
import { MUTATION_KEYS } from "@consts";
import { MetadataValidationDTO } from "@models";

export const useValidateMutation = () => {
  const { data, isLoading, mutateAsync } = useMutation({
    mutationFn: (body: MetadataValidationDTO) => postValidate(body),
    mutationKey: [MUTATION_KEYS.postValidateKey],
  });

  return {
    validateMetadata: mutateAsync,
    validationStatus: data,
    isValidating: isLoading,
  };
};
