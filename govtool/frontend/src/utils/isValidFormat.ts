import {
  Address,
  DRep,
  RewardAddress,
} from "@emurgo/cardano-serialization-lib-asmjs";
import i18n from "@/i18n";
import { adaHandleService } from "@/services/AdaHandle";

export const URL_REGEX =
  /^(?:(?:https?:\/\/)?(?:\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|(?:[a-zA-Z0-9-]+\.)+[a-zA-Z]{2,})(?:\/[^\s]*)?)|(?:ipfs:\/\/(?:[a-zA-Z0-9]+(?:\/[a-zA-Z0-9._-]+)*))$|^$/;
export const HASH_REGEX = /^[0-9A-Fa-f]+$/;
export const PAYMENT_ADDRESS_REGEX = /addr1[a-z0-9]+/i;
export const IMAGE_REGEX =
  /^(https?:\/\/[^\s]+\.(?:png|jpg|jpeg|gif|bmp|webp|svg)(\?.*)?$|https?:\/\/[^\s]+$|data:image\/(?:png|jpeg|gif|bmp|webp|svg\+xml);base64,[A-Za-z0-9+/]+={0,2}$)/;

export function isValidURLFormat(str: string) {
  if (!str.length) return false;
  return URL_REGEX.test(str);
}

export function isValidHashFormat(str: string) {
  if (!str.length) return false;
  return HASH_REGEX.test(str);
}

export function isValidURLLength(s: string) {
  if (s.length > 128) {
    return i18n.t("forms.errors.tooLongUrl");
  }

  const encoder = new TextEncoder();
  const byteLength = encoder.encode(s).length;

  return byteLength <= 128 ? true : i18n.t("forms.errors.tooLongUrl");
}

export async function isRewardAddress(address: string) {
  try {
    const stake = RewardAddress.from_address(Address.from_bech32(address));
    return stake ? true : i18n.t("forms.errors.mustBeStakeAddress");
  } catch (e) {
    return i18n.t("forms.errors.mustBeStakeAddress");
  }
}

export async function isReceivingAddress(address?: string) {
  try {
    if (!address) {
      return true;
    }

    const receivingAddress = Address.from_bech32(address);
    if (receivingAddress) {
      return true;
    }
    const isValidAdaHandle = await adaHandleService.isValidAdaHandle(address);

    if (isValidAdaHandle) {
      return true;
    }

    return i18n.t("forms.errors.mustBeReceivingAddress");
  } catch (e) {
    return i18n.t("forms.errors.mustBeReceivingAddress");
  }
}

export async function isDRepView(view?: string) {
  if (!view) {
    return true;
  }
  if (DRep.from_bech32(view)) {
    return true;
  }
  return i18n.t("forms.errors.mustBeDRepView");
}
