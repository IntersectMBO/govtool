import { TxSubmitResponse } from "@services/kuberService";
import * as fs from "fs";
import * as lockfile from "lockfile";
import { Logger } from "../../cypress/lib/logger/logger";

import path = require("path");

export class LockInterceptor {
  private static async acquireLock(
    address: string,
    message?: string
  ): Promise<void> {
    const lockFilePath = path.resolve(__dirname, `../.lock-pool/${address}`);

    try {
      await log(
        `${address} -> acquiring lock` + message && `\nMessage: ${message}`
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
      await log(`${address} -> acquired lock`);
    } catch (err) {
      Logger.fail("Failed to write lock logs");
    }
  }

  private static async releaseLock(
    address: string,
    message?: string
  ): Promise<void> {
    const lockFilePath = path.resolve(__dirname, `../.lock-pool/${address}`);

    try {
      await log(
        `${address} -> releasing lock` + message && `\nMessage: ${message}`
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
      await log(`${address} -> released lock\n`);
    } catch (err) {
      Logger.fail("Failed to write lock logs");
    }
  }

  private static async waitForReleaseLock(
    address: string,
    message?: string
  ): Promise<void> {
    const pollInterval = 200;

    try {
      await log(
        `${address} -> waiting lock` + message && `\nMessage: ${message}`
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
      Logger.fail("Failed to write lock logs");
    }
  }

  static async intercept(
    address: string,
    callbackFn: () => Promise<TxSubmitResponse>,
    message?: string
  ): Promise<TxSubmitResponse> {
    const isAddressLocked = checkAddressLock(address);
    if (isAddressLocked) {
      await LockInterceptor.waitForReleaseLock(address, message);
    }

    await LockInterceptor.acquireLock(address, message);
    try {
      const res = await callbackFn();
      return { ...res, address };
    } catch (err) {
      await LockInterceptor.releaseLock(address, "Tx failure");
      throw err;
    }
  }

  static async releaseLockForAddress(address: string) {
    await this.releaseLock(address);
  }
}

function checkAddressLock(address: string): boolean {
  const lockFilePath = path.resolve(__dirname, `../.lock-pool/${address}`);
  return lockfile.checkSync(lockFilePath);
}

function log(message: string): Promise<void> {
  const logFilePath = path.resolve(__dirname, "../.logs/lock_logs.txt");
  const logMessage = `[${new Date().toISOString()}] ${message}\n`;
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
