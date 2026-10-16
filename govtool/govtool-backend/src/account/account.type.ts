export type AccountInfoRow = {
  id: number | string;
  view: string;
  is_script_based: boolean;
  is_registered: boolean;
};

export type AccountInfoResponse = {
  /** Null under every provider: the contract carries no internal row id. */
  id: number | null;
  view: string;
  isRegistered: boolean;
  isScriptBased: boolean;
};
