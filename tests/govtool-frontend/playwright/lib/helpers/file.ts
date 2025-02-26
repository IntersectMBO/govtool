import { readFile, writeFile } from "fs";
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

export async function getFile(fileName: string): Promise<any> {
  const data: string = await new Promise((resolve, reject) =>
    readFile(`${baseFilePath}/${fileName}`, "utf8", (err, data) => {
      if (err) {
        if (err.code === "ENOENT") {
          resolve(undefined);
        } else {
          reject(err);
        }
      } else {
        resolve(data);
      }
    })
  );
  return data ? JSON.parse(data) : undefined;
}
