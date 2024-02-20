export const WALLET_LS_KEY = "wallet_data";
export const DELEGATE_TRANSACTION_KEY = "delegate_transaction";
export const REGISTER_TRANSACTION_KEY = "register_transaction";
export const REGISTER_SOLE_VOTER_TRANSACTION_KEY =
  "register_sole_voter_transaction";
export const DELEGATE_TO_KEY = "delegate_to_transaction";
export const PROTOCOL_PARAMS_KEY = "protocol_params";
export const SANCHO_INFO_KEY = "sancho_info";
export const VOTE_TRANSACTION_KEY = "vote_transaction";

export function getItemFromLocalStorage(key: string) {
  const item = window.localStorage.getItem(key);
  return item ? JSON.parse(item) : null;
}

export function setItemToLocalStorage(key: string, data: any) {
  window.localStorage.setItem(key, JSON.stringify(data));
}

export function removeItemFromLocalStorage(key: string) {
  window.localStorage.removeItem(key);
}
