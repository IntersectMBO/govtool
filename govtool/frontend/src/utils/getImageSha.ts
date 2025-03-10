/**
 * Retrieves the SHA-256 hash of an image from the specified URL.
 * @param imageUrl - The URL of the image.
 * @returns The SHA-256 hash of the image.
 * @throws If the image URL is invalid or if there is an error fetching the image.
 */
export const getImageSha = async (imageUrl: string) => {
  try {
    const response = await fetch(imageUrl, {
      // Required to not being blocked by APIs that require a User-Agent
      headers: {
        "User-Agent": "GovTool/image-sha",
      },
    });
    if (!response.ok)
      throw new Error(`Failed to fetch image: ${response.statusText}`);

    const imageBuffer = await response.arrayBuffer();
    const hashBuffer = await crypto.subtle.digest("SHA-256", imageBuffer);
    const hashArray = Array.from(new Uint8Array(hashBuffer));
    const hashHex = hashArray
      .map((b) => b.toString(16).padStart(2, "0"))
      .join("");

    return hashHex;
  } catch (error) {
    throw new Error(`Failed to process image: ${(error as Error).message}`);
  }
};
