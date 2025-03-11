import { NetworkMetrics } from "@/models";
import { HandleObject } from "@/models/adaHandle";

export const ADAHANDLE_BASE_URL = {
  sanchonet: "",
  preview: "https://preview.api.handle.me",
  testnet: "",
  preprod: "https://preprod.api.handle.me",
  mainnet: "https://api.handle.me",
};

/**
 * Service for interacting with AdaHandle API.
 */
class AdaHandleService {
  private static instance: AdaHandleService;

  private adaHandleBaseUrl: string | null = null;

  /**
   * Private constructor to enforce singleton pattern.
   */
  // eslint-disable-next-line no-useless-constructor, no-empty-function
  private constructor() {}

  /**
   * Get the instance of AdaHandleService.
   * @returns The instance of AdaHandleService.
   */
  static getInstance(): AdaHandleService {
    if (!AdaHandleService.instance) {
      AdaHandleService.instance = new AdaHandleService();
    }
    return AdaHandleService.instance;
  }

  /**
   * Initialize the AdaHandleService with the base URL for a specific network.
   * @param network - The name of the network.
   */
  initialize(network: NetworkMetrics["networkName"]): void {
    if (this.adaHandleBaseUrl !== ADAHANDLE_BASE_URL[network]) {
      this.adaHandleBaseUrl = ADAHANDLE_BASE_URL[network];
    }
  }

  /**
   * Get the details of a handle from the AdaHandle API.
   * @param handle - The handle to retrieve details for.
   * @returns A Promise that resolves to the HandleObject or null if the handle is not found.
   */
  async getHandleDetails(handle: string): Promise<HandleObject | null> {
    if (!this.adaHandleBaseUrl) {
      throw new Error("AdaHandleService is not initialized with a network.");
    }
    if (!handle) {
      return null;
    }

    if (handle.startsWith("$")) {
      handle = handle.slice(1);
    }

    try {
      const response = await fetch(
        `${this.adaHandleBaseUrl}/handles/${handle}`,
      );
      if (response.ok) {
        return response.json();
      }
      return null;
    } catch (error) {
      console.error("Error fetching handle:", error);
      return null;
    }
  }

  /**
   * Check if an Ada handle is valid.
   * @param handle - The handle to check.
   * @returns A Promise that resolves to true if the handle is valid, false otherwise.
   */
  async isValidAdaHandle(handle: string): Promise<boolean> {
    const handleObject = await this.getHandleDetails(handle);
    return Boolean(handleObject);
  }

  /**
   * Get the CIP-105 DRep ID associated with an Ada handle.
   * @param handle - The handle to get the CIP-105 DRep ID for.
   * @returns A Promise that resolves to the CIP-105 DRep ID or an empty string if not found.
   */
  async getAdaHandleCIP105DRepId(handle: string): Promise<string> {
    const handleObject = await this.getHandleDetails(handle);
    return handleObject?.drep?.cip_105 ?? "";
  }

  /**
   * Get the CIP-129 DRep ID associated with an Ada handle.
   * @param handle - The handle to get the CIP-129 DRep ID for.
   * @returns A Promise that resolves to the CIP-129 DRep ID or an empty string if not found.
   */
  async getAdaHandleCIP129DRepId(handle: string): Promise<string> {
    const handleObject = await this.getHandleDetails(handle);
    return handleObject?.drep?.cip_129 ?? "";
  }
}

export const adaHandleService = AdaHandleService.getInstance();
