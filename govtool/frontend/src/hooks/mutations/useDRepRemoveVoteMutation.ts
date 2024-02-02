import { postDRepRemoveVote } from "@services";
import { useMutation } from "react-query";

export const useDRepRemoveVoteMutation = () => {
  const { mutateAsync } = useMutation(postDRepRemoveVote);

  return {
    removeVote: mutateAsync,
  };
};
