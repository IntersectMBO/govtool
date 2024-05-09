import { TxSubmitResponse } from "@services/kuberService";
import * as fs from "fs";
import * as lockfile from "lockfile";
import { Logger } from "../../cypress/lib/logger/logger";

import path = require("path");

export interface LockInterceptorInfo {
  lockId: string;
  address: string;
}

abstract class BaseLock {
  abstract acquireLock(key: string, id?: string): Promise<boolean>;

  abstract releaseLock(key: string, id?: string): Promise<boolean>;

  abstract checkLock(key: string): Promise<boolean>;
}

export class LockInterceptor {
  private static async acquireLock(
    address: string,
    lockId: string
  ): Promise<void> {
    const lockFilePath = path.resolve(__dirname, `../.lock-pool/${address}`);

    try {
      await log(
        `Initiator: ${address} \n---------------------> acquiring lock for:${lockId}`
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
        `Initiator: ${address} \n---------------------> acquired lock for:${lockId}`
      );
    } catch (err) {
      throw err;
    }
  }

  private static async releaseLock(
    address: string,
    lockId: string
  ): Promise<void> {
    const lockFilePath = path.resolve(__dirname, `../.lock-pool/${address}`);

    try {
      await log(
        `Initiator: ${address} \n---------------------> releasing lock for:${lockId}`
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
        `Initiator: ${address} \n---------------------> released lock for:${lockId}\n`
      );
    } catch (err) {
      throw err;
    }
  }

  private static async waitForReleaseLock(
    address: string,
    lockId: string
  ): Promise<void> {
    const pollInterval = 4000; // 4 secs

    try {
      await log(
        `Initiator: ${address} \n ---------------------> waiting lock for:${lockId}`
      );
      return new Promise<void>((resolve, reject) => {
        const pollFn = () => {
          try {
            const isAddressLocked = checkAddressLock(address);
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

  static async intercept(
    address: string,
    callbackFn: () => Promise<TxSubmitResponse>,
    lockId: string,
    provider: "local" | "server" = "local"
  ): Promise<TxSubmitResponse> {
    while (true) {
      const isAddressLocked = checkAddressLock(address);
      if (isAddressLocked) {
        await LockInterceptor.waitForReleaseLock(address, lockId);
      }

      try {
        await LockInterceptor.acquireLock(address, lockId);
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
      return { ...res, lockInfo: { lockId, address } };
    } catch (err) {
      const errorMessage = { lock_id: lockId, error: JSON.stringify(err) };
      await log(`Task failure: \n${JSON.stringify(errorMessage)}`);
      await LockInterceptor.releaseLock(address, lockId);
      throw err;
    }
  }

  static async releaseLockForAddress(
    address: string,
    lockId: string,
    message?: string
  ) {
    try {
      message && (await log(message));

      await this.releaseLock(address, lockId);
    } catch {
      Logger.fail("Failed to write lock logs");
    }
  }
}

function checkAddressLock(address: string): boolean {
  const lockFilePath = path.resolve(__dirname, `../.lock-pool/${address}`);
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
  const logFilePath = path.resolve(__dirname, "../.logs/lock_logs.txt");
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
