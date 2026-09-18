export type AccountInfoRow = {
  id: number | string;
  view: string;
  is_script_based: boolean;
  is_registered: boolean;
};

export type AccountInfoResponse = {
  id: number;
  view: string;
  isRegistered: boolean;
  isScriptBased: boolean;
};
