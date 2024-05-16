import { Download } from "@playwright/test";
import * as fs from "fs";

export async function downloadMetadata(download: Download): Promise<{
  name: string;
  data: JSON;
}> {
  const path = `.download/${download.suggestedFilename()}`;
  await download.saveAs(path);
  const fileContent = fs.readFileSync(path, "utf-8");
  const jsonData = JSON.parse(fileContent);
  return { name: download.suggestedFilename(), data: jsonData };
}
