import { postAdaHolderDelegate } from "@services";
import { useMutation } from "react-query";

export const useAdaHolderDelegateMutation = () => {
  const { mutateAsync } = useMutation(postAdaHolderDelegate, {});

  return {
    delegateAsAdaHolder: mutateAsync,
  };
};
