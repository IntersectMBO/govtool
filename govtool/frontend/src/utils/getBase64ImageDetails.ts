/**
 * Checks if a given string is a base64-encoded image or SVG and returns its details.
 *
 * @param str - The string to check.
 * @returns An object with the MIME type, base64 prefix, and validity flag.
 */
export function getBase64ImageDetails(str: string): {
  type: string | null;
  base64Prefix: string | null;
  isValidBase64Image: boolean;
} {
  if (!str || typeof str !== "string") {
    return {
      type: null,
      base64Prefix: null,
      isValidBase64Image: false,
    };
  }

  try {
    const decoded = atob(str);

    if (decoded.trim().startsWith("<?xml") || decoded.includes("<svg")) {
      return {
        type: "svg+xml",
        base64Prefix: "data:image/svg+xml;base64,",
        isValidBase64Image: true,
      };
    }

    const magicNumbers = decoded.slice(0, 4);
    let type: string | null = null;

    switch (magicNumbers) {
      case "\x89PNG":
        type = "png";
        break;
      case "\xFF\xD8\xFF":
        type = "jpeg";
        break;
      case "GIF8":
        type = "gif";
        break;
      default:
        type = null;
    }

    if (type) {
      return {
        type,
        base64Prefix: `data:image/${type};base64,`,
        isValidBase64Image: true,
      };
    }
  } catch {
    return {
      type: null,
      base64Prefix: null,
      isValidBase64Image: false,
    };
  }

  return {
    type: null,
    base64Prefix: null,
    isValidBase64Image: false,
  };
}
