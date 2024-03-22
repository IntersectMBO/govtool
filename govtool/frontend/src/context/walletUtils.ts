import { getAdaHolderCurrentDelegation, getVoterInfo } from "@services";
import { VoterInfo } from "@models";
import { DRepActionType } from "./wallet";

export const setLimitedRegistrationInterval = (
  intervalTime: number,
  attemptsNumber: number,
  dRepID: string,
  transactionType: DRepActionType | Omit<DRepActionType, "update">,
  setVoter: (key: undefined | VoterInfo) => void,
): Promise<boolean> =>
  new Promise((resolve) => {
    const desiredResult = transactionType === "registration";
    let count = 0;

    const interval = setInterval(async () => {
      if (count < attemptsNumber) {
        // TODO: Refactor this to not iterate over the same data
        count++;

        try {
          const data = await getVoterInfo(dRepID);

          if (
            data.isRegisteredAsDRep === desiredResult ||
            data.isRegisteredAsSoleVoter === desiredResult
          ) {
            setVoter(data);
            clearInterval(interval);
            resolve(desiredResult);
          }
        } catch (error) {
          clearInterval(interval);
          resolve(!desiredResult);
        }
      } else {
        clearInterval(interval);
        resolve(!desiredResult);
      }
    }, intervalTime);
  });

export const setLimitedDelegationInterval = (
  intervalTime: number,
  attemptsNumber: number,
  dRepID: string,
  delegateTo: string,
  stakeKey?: string,
): Promise<boolean> =>
  new Promise((resolve) => {
    let count = 0;

    const interval = setInterval(async () => {
      if (count < attemptsNumber) {
        count++;

        try {
          const currentDelegation = await getAdaHolderCurrentDelegation({
            stakeKey,
          });

          if (
            (delegateTo === dRepID && currentDelegation === dRepID) ||
            (delegateTo === "no confidence" &&
              currentDelegation === "drep_always_no_confidence") ||
            (delegateTo === "abstain" &&
              currentDelegation === "drep_always_abstain") ||
            (delegateTo !== dRepID && delegateTo === currentDelegation)
          ) {
            clearInterval(interval);
            resolve(true);
          }
        } catch (error) {
          clearInterval(interval);
          resolve(false);
        }
      } else {
        clearInterval(interval);
        resolve(false);
      }
    }, intervalTime);
  });
