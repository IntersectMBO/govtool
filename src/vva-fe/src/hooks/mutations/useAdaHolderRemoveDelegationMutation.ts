import { postAdaHolderRemoveDelegation } from "@services";
import { useMutation } from "react-query";

export const useAdaHolderRemoveDelegation = () => {
  const { mutateAsync } = useMutation(postAdaHolderRemoveDelegation);

  return {
    register: mutateAsync,
  };
};
