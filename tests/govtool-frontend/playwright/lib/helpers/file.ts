import { readFile, rm, writeFile } from "fs";
const path = require("path");

const mockFolderPath = path.resolve(__dirname, "../_mock");
const basePath = path.join(__dirname, "../..");

export async function createFile(fileName: string, data?: any) {
  await new Promise<void>((resolve, reject) =>
    writeFile(
      `${mockFolderPath}/${fileName}`,
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
    readFile(`${mockFolderPath}/${fileName}`, "utf8", (err, data) => {
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

export async function deleteFile(fileName: string): Promise<void> {
  await new Promise<void>((resolve, reject) =>
    rm(`${mockFolderPath}/${fileName}`, (err) => {
      if (err) {
        if (err.code === "ENOENT") {
          resolve();
        } else {
          reject(err);
        }
      } else {
        resolve();
      }
    })
  );
}

export async function deleteFolder(folderName: string): Promise<void> {
  await new Promise<void>((resolve, reject) =>
    rm(`${basePath}/${folderName}`, { recursive: true, force: true }, (err) => {
      if (err) {
        if (err.code === "ENOENT") {
          resolve();
        } else {
          reject(err);
        }
      } else {
        resolve();
      }
    })
  );
}
