import environments from "@constants/environments";
import { Logger } from "@helpers/logger";
import {
  AddCommentPayload,
  AddPollPayload,
  ProposalCreationPayload,
} from "@types";

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

      const response = await res.json();
      if (res.status === 200) {
        Logger.success("Governance action proposal deleted successfully");
        return response;
      }

      Logger.fail("Failed to delete governance action proposal");
      throw new Error(response["error"]["message"]);
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

      const response = await res.json();
      console.log(response);
      if (res.status === 200) {
        Logger.success("Poll added successfully");
        return response;
      }

      Logger.fail("Failed to add poll  governance action proposal");
      throw new Error(response["error"]["message"]);
    } catch (err) {
      Logger.fail("Failed to add poll governance action proposal");
      throw err;
    }
  },

  addComment: async (data: AddCommentPayload) => {
    try {
      const res = await fetch(`${environments.pdfUrl}/api/comments`, {
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
        Logger.success("Comment added successfully");
        return response;
      }

      Logger.fail("Failed to add comment on  governance action proposal");
      throw new Error(response["error"]["message"]);
    } catch (err) {
      Logger.fail("Failed to add comment on  governance action proposal");
      throw err;
    }
  },
};

export default proposalDiscussionService;
