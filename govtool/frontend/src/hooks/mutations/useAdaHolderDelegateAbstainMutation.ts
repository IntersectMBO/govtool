import { postAdaHolderDelegateAbstain } from "@services";
import { useMutation } from "react-query";

export const useAdaHolderDelegateAbstainMutation = () => {
  const { mutateAsync } = useMutation(postAdaHolderDelegateAbstain, {});

  return {
    delegateAbstainAsAdaHolder: mutateAsync,
  };
};
