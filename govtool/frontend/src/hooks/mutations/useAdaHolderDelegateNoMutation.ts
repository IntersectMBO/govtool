import { postAdaHolderDelegateNo } from "@services";
import { useMutation } from "react-query";

export const useAdaHolderDelegateNoMutation = () => {
  const { mutateAsync } = useMutation(postAdaHolderDelegateNo, {});

  return {
    delegateNoAsAdaHolder: mutateAsync,
  };
};
