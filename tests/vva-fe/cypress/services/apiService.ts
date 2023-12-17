import { randomBytes } from "crypto";
import { Logger } from "../lib/logger/logger";
import blockfrostService from "./blockfrostService";
import kuberService from "./kuberService";

const apiUrl = (function () {
  let url: string = Cypress.env("apiUrl");
  return url.endsWith("/") ? url.substring(0, url.length - 1) : url;
})();

function getApi(url: string): Cypress.Chainable<Cypress.Response<any>> {
  const normalizedUrl = url.startsWith("/") ? url : "/" + url;
  return cy.request(apiUrl + normalizedUrl);
}
function postApi(
  url: string,
  body?: Cypress.RequestBody
): Cypress.Chainable<Cypress.Response<any>> {
  const normalizedUrl = url.startsWith("/") ? url : "/" + url;
  return cy.request("POST", apiUrl + normalizedUrl, body);
}

export const pollTxWithKuber = async (
  txHash: string,
  maxRetries = 32
): Promise<void> => {
  const MAX_RETRIES = maxRetries;
  const RETRY_INTERVAL = 5000;

  if (txHash === undefined) return;

  function retry(tries, cb) {
    if (tries) {
      setTimeout(() => checkTransaction(tries - 1, cb), RETRY_INTERVAL);
    } else {
      cb(
        new Error(
          "[Kuber Tx] Wait timeout for tx:" +
            txHash +
            ", waited for " +
            (MAX_RETRIES * RETRY_INTERVAL) / 1000 +
            "secs"
        )
      );
    }
  }

  function checkTransaction(tries, cb) {
    return kuberService
      .getTransactionDetails(txHash)
      .then(async (res) => {
        if ((await res.json()).length > 0) {
          console.debug("[Kuber Poll Tx] Successful", res);
          cb();
        } else if (res.status === 500) {
          throw cb(new Error("Internal Server Error"));
        } else {
          Logger.fail(
            "[Kuber Poll Tx] Tx unsuccessful waiting for another " +
              RETRY_INTERVAL / 1000 +
              "secs..."
          );

          retry(tries, cb);
        }
      })
      .catch((e) => {
        if (e.status == 404) {
          retry(tries, cb);
        } else {
          throw cb(
            new Error(
              "[Kuber Poll Tx] Timeout for tx:" +
                txHash +
                ", waited for " +
                (MAX_RETRIES * RETRY_INTERVAL) / 1000 +
                "secs"
            )
          );
        }
      });
  }

  return new Promise((resolve, reject) => {
    setTimeout(() => {
      checkTransaction(MAX_RETRIES, (err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      });
    });
    RETRY_INTERVAL;
  });
};

export const pollTx = async (txHash: string): Promise<void> => {
  const MAX_RETRIES = 20;
  const RETRY_INTERVAL = 5000;

  function checkTransaction(tries, cb) {
    return blockfrostService
      .getTransactionDetails(txHash)
      .then((res) => {
        console.debug("[Blockfrost Poll Tx] successful", res);
        cb();
      })
      .catch((e) => {
        Logger.fail(
          "[Blockfrost Poll Tx] Unsuccessful waiting for another " +
            RETRY_INTERVAL / 1000 +
            "secs..."
        );

        if (e.status == 404) {
          if (tries) {
            setTimeout(() => checkTransaction(tries - 1, cb), RETRY_INTERVAL);
          } else {
            cb(
              new Error(
                "[Blockfrost Poll Tx] Timeout for tx:" +
                  txHash +
                  ", waited for " +
                  (MAX_RETRIES * RETRY_INTERVAL) / 1000 +
                  "secs"
              )
            );
          }
        } else {
          cy.then(async () => {
            expect(e.status).to.closeTo(200, 299);
          });
          throw cb(e);
        }
      });
  }

  return new Promise((resolve, reject) => {
    setTimeout(() => {
      checkTransaction(MAX_RETRIES, (err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      });
    }),
      RETRY_INTERVAL;
  });
};
