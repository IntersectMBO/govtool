import environments from "@constants/environments";
import { Logger } from "@helpers/logger";
import { AddPollPayload, ProposalCreationPayload } from "@types";

import fetch = require("node-fetch");

const proposalDiscussionService = {
  createProposal: async (data: { data: ProposalCreationPayload }) => {
    try {
      const res = await fetch(`${environments.pdfUrl}/api/proposals`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          authorization:
            "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MzMsImlhdCI6MTcxODE2NjA5NiwiZXhwIjoxNzIwNzU4MDk2fQ.oWJefxnDGosktBlPQTvJ01Xqxa8YVAuhYs9MQPJE9po",
        },
        body: JSON.stringify(data),
      });

      const response = await res.json();
      if (res.status === 200) {
        console.log(JSON.stringify(data));
        Logger.success("Governance action proposal created successfully");
        return response;
      }

      Logger.fail("Failed to create governance action proposal");
      throw new Error(response["error"]["message"]);
    } catch (err) {
      Logger.fail("Failed to create governance action proposal");
      throw err;
    }
  },
  deleteProposal: async (proposalId: number) => {
    try {
      const res = await fetch(
        `${environments.pdfUrl}/api/proposals/${proposalId}`,
        {
          method: "DELETE",
          headers: {
            "content-type": "application/json",
            authorization:
              "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MzMsImlhdCI6MTcxODE2NjA5NiwiZXhwIjoxNzIwNzU4MDk2fQ.oWJefxnDGosktBlPQTvJ01Xqxa8YVAuhYs9MQPJE9po",
          },
        }
      );
      await res.json();
      Logger.success("Governance action proposal deleted successfully");
    } catch (err) {
      Logger.fail("Failed to delete governance action proposal");
      throw err;
    }
  },

  addPoll: async (data: AddPollPayload) => {
    try {
      const res = await fetch(`${environments.pdfUrl}/api/polls`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          authorization:
            "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MzMsImlhdCI6MTcxODE2NjA5NiwiZXhwIjoxNzIwNzU4MDk2fQ.oWJefxnDGosktBlPQTvJ01Xqxa8YVAuhYs9MQPJE9po",
        },
        body: JSON.stringify(data),
      });
      await res.json();
      Logger.success("Poll added successfully");
    } catch (err) {
      Logger.fail("Failed to delete governance action proposal");
      throw err;
    }
  },
};

export default proposalDiscussionService;
