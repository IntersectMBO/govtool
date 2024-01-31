import { useMutation } from "react-query";
import { useCardano, useSnackbar } from "@context";
import { postDRepRetire } from "@services";

export const useDRepRetireMutation = () => {
  const { setDRep } = useCardano();
  const { addSuccessAlert } = useSnackbar();

  const { mutateAsync } = useMutation(postDRepRetire, {
    onSuccess: () => {
      setDRep({ deposit: 100, wasRegistered: true, isRegistered: false });
      addSuccessAlert("DRep retired.");
    },
  });

  return {
    retire: mutateAsync,
  };
};
