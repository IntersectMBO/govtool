import { useMutation } from "react-query";
import { useCardano } from "@context";
import { postDRepRegister } from "@services";

export const useDRepRegisterMutation = () => {
  const { setDRep } = useCardano();

  const { mutateAsync, isLoading } = useMutation(postDRepRegister, {
    onSuccess: () => {
      setDRep({
        deposit: 100,
        isRegistered: true,
        wasRegistered: false,
      });
    },
  });

  return {
    isLoading: isLoading,
    register: mutateAsync,
  };
};
