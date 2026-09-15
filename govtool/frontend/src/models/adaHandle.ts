export type ResolvedAddresses = {
  ada?: string;
  eth?: string;
  btc?: string;
};

export type Virtual = {
  expires_time: number;
  public_mint: boolean;
};

export type HandleObject = {
  hex: string;
  name: string;
  handle_type?: "handle" | "nft_subhandle";
  virtual?: Virtual;
  holder: string;
  holder_type: "wallet" | "drep";
  image: string;
  standard_image: string;
  image_hash: string;
  standard_image_hash: string;
  length: number;
  og?: number;
  og_number: number;
  rarity: string;
  characters: string;
  numeric_modifiers: string;
  sub_length: number;
  sub_rarity: string;
  sub_characters: string;
  sub_numeric_modifiers: string;
  payment_key_hash: string;
  default_in_wallet: string;
  pfp_image: string;
  bg_image: string;
  pfp_asset?: string;
  bg_asset?: string;
  resolved_addresses: ResolvedAddresses;
  original_address?: string;
  version: number;
  svg_version: string;
  utxo: string;
  lovelace?: number;
  has_datum: boolean;
  created_slot_number: number;
  updated_slot_number: number;
  last_update_address?: string;
  pz_enabled?: boolean;
  last_edited_time?: number;
  drep?: {
    type: "drep";
    cred: string;
    hex: string;
    cip_105: string;
    cip_129: string;
  };
};
