import { Logger } from "@helpers/logger";
import * as fs from "fs";
import * as lockfile from "lockfile";

import path = require("path");

export interface LockInterceptorInfo {
  lockId: string;
  initiator: string;
}

export class LockInterceptor {
  private static async acquireLock(
    initiator: string,
    lockId: string
  ): Promise<void> {
    const lockFilePath = path.resolve(__dirname, `../${initiator}`);

    try {
      await log(
        `Initiator: ${initiator} \n---------------------> acquiring lock for:${lockId}`
      );
      await new Promise<void>((resolve, reject) => {
        lockfile.lock(lockFilePath, (err) => {
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        });
      });
      await log(
        `Initiator: ${initiator} \n---------------------> acquired lock for:${lockId}`
      );
    } catch (err) {
      throw err;
    }
  }

  private static async _releaseLock(
    initiator: string,
    lockId: string
  ): Promise<void> {
    const lockFilePath = path.resolve(__dirname, `../${initiator}`);

    try {
      await log(
        `Initiator: ${initiator} \n---------------------> releasing lock for:${lockId}`
      );
      await new Promise<void>((resolve, reject) => {
        lockfile.unlock(lockFilePath, async (err) => {
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        });
      });
      await log(
        `Initiator: ${initiator} \n---------------------> released lock for:${lockId}\n`
      );
    } catch (err) {
      throw err;
    }
  }

  private static async waitForReleaseLock(
    initiator: string,
    lockId: string
  ): Promise<void> {
    const pollInterval = 100; // 100 milliseconds

    try {
      await log(
        `Initiator: ${initiator} \n ---------------------> waiting lock for:${lockId}`
      );
      return new Promise<void>((resolve, reject) => {
        const pollFn = () => {
          try {
            const isAddressLocked = checkLock(initiator);
            if (!isAddressLocked) {
              resolve();
            } else {
              setTimeout(pollFn, pollInterval);
            }
          } catch (err) {
            reject(err);
          }
        };

        pollFn();
      });
    } catch (err) {
      throw err;
    }
  }

  static async intercept<T>(
    initiator: string,
    lockId: string,
    callbackFn: () => Promise<T>
  ): Promise<T> {
    while (true) {
      const isAddressLocked = checkLock(initiator);
      if (isAddressLocked) {
        await LockInterceptor.waitForReleaseLock(initiator, lockId);
      }

      try {
        await LockInterceptor.acquireLock(initiator, lockId);
        break;
      } catch (err) {
        if (err.code === "EEXIST") {
          await new Promise((resolve) => setTimeout(resolve, 1000)); // timeout for retry
          continue;
        } else {
          throw err;
        }
      }
    }
    try {
      const res = await callbackFn();
      return { ...res, lockInfo: { lockId, initiator } };
    } catch (err) {
      const errorMessage = { lock_id: lockId, error: JSON.stringify(err) };
      await log(`Task failure: \n${JSON.stringify(errorMessage)}`);
      await LockInterceptor._releaseLock(initiator, lockId);
      throw err;
    }
  }

  static async releaseLock(
    interceptor: string,
    lockId: string,
    message?: string
  ) {
    try {
      message && (await log(message));

      await this._releaseLock(interceptor, lockId);
    } catch {
      Logger.fail("Failed to write lock logs");
    }
  }
}

function checkLock(initiator: string): boolean {
  const lockFilePath = path.resolve(__dirname, `../${initiator}`);
  return lockfile.checkSync(lockFilePath);
}

function log(message: string): Promise<void> {
  const options: Intl.DateTimeFormatOptions = {
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: false,
    timeZone: "Asia/Kathmandu",
  };
  const logFilePath = path.resolve(__dirname, "../lock_logs.txt");
  const logMessage = `[${new Date().toLocaleString("en-US", options)}] ${message}\n`;
  return new Promise((resolve, reject) => {
    fs.appendFile(logFilePath, logMessage, (err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}
