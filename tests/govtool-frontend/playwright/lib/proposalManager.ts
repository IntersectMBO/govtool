import { proposalCreationPayload } from "@types";
import * as fs from "fs";
const path = require("path");

const baseFilePath = path.resolve(__dirname, "./_mock");

class ProposalManager {
  private static instance: ProposalManager;

  public static getInstance(): ProposalManager {
    if (!ProposalManager.instance) {
      ProposalManager.instance = new ProposalManager();
    }
    return ProposalManager.instance;
  }

  async writeProposal(response: Object) {
    await new Promise<void>((resolve, reject) =>
      fs.writeFile(
        `${baseFilePath}/proposal.json`,
        JSON.stringify(response, null, 2),
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

  private async readProposal(): Promise<Object> {
    const data: string = await new Promise((resolve, reject) =>
      fs.readFile(`${baseFilePath}/proposal.json`, "utf8", (err, data) => {
        if (err) {
          reject(err);
        } else {
          resolve(data);
        }
      })
    );
    return JSON.parse(data);
  }

  async getProposalId(): Promise<number> {
    const proposal = await this.readProposal();
    return proposal["response"]["data"]["attributes"]["proposal_id"];
  }

  async getProposalPayload(): Promise<proposalCreationPayload> {
    const proposal = await this.readProposal();
    return proposal["payload"]["data"];
  }
}

export default ProposalManager.getInstance();
