import { writeFile } from "fs";
const path = require("path");

const baseFilePath = path.resolve(__dirname, "../_mock");

export async function createFile(fileName: string, data?: any) {
  await new Promise<void>((resolve, reject) =>
    writeFile(
      `${baseFilePath}/${fileName}`,
      JSON.stringify(data, null, 2),
      (err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      }
    )
  );
}
