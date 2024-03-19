import { NodeObject } from "jsonld";

export const downloadJson = (json: NodeObject, fileName?: string) => {
  const jsonString = `data:text/jsonld;chatset=utf-8,${encodeURIComponent(
    JSON.stringify(json, null, 2),
  )}`;
  const link = document.createElement("a");
  link.href = jsonString;
  link.download = `${fileName || "data"}.jsonld`;

  link.click();
};
