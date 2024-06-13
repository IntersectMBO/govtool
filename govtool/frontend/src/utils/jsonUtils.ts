import { NodeObject } from "jsonld";

/**
 * Downloads a JSON object as a file.
 * @param json - The JSON object to be downloaded.
 * @param fileName - The name of the file to be downloaded.
 * If not provided, the default name will be "data.jsonld".
 */
export const downloadJson = (json: NodeObject, fileName?: string) => {
  const jsonString = `data:text/jsonld;charset=utf-8,${encodeURIComponent(
    JSON.stringify(json, null, 2),
  )}`;
  const link = document.createElement("a");
  link.href = jsonString;
  link.download = `${fileName || "data"}.jsonld`;

  link.click();
};

/**
 * Downloads a text file with the given content.
 * @param text - The content of the text file.
 * @param fileName - The name of the file (optional).
 * If not provided, the default name will be "data.txt".
 */
export const downloadTextFile = (text: string, fileName?: string) => {
  const blob = new Blob([text], { type: "text/utf-8" });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = `${fileName || "data"}.txt`;

  link.click();
};
