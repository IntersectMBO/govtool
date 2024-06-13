import environments from "@constants/environments";
import { Logger } from "@helpers/logger";
import { RequestInit } from "node-fetch";
import { CommentRequest, PollRequest, ProposalCreateRequest } from "./types";

import fetch = require("node-fetch");

export const postCreateProposal = async (data: ProposalCreateRequest) => {
  const endpoint = "/proposals";
  const options: RequestInit = {
    method: "POST",
    headers: {
      "content-type": "application/json",
      authorization:
        "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MzMsImlhdCI6MTcxODE2NjA5NiwiZXhwIjoxNzIwNzU4MDk2fQ.oWJefxnDGosktBlPQTvJ01Xqxa8YVAuhYs9MQPJE9po",
    },
    body: JSON.stringify({ data: { data } }),
  };

  return fetchClient(endpoint, options);
};

export const deleteProposal = async (proposalId: number) => {
  const endpoint = `/proposals/${proposalId}`;
  const options: RequestInit = {
    method: "DELETE",
    headers: {
      "content-type": "application/json",
      authorization:
        "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MzMsImlhdCI6MTcxODE2NjA5NiwiZXhwIjoxNzIwNzU4MDk2fQ.oWJefxnDGosktBlPQTvJ01Xqxa8YVAuhYs9MQPJE9po",
    },
  };

  return fetchClient(endpoint, options);
};

export const postAddPoll = async (data: PollRequest) => {
  const endpoint = `/api/polls`;
  const options: RequestInit = {
    method: "DELETE",
    headers: {
      "content-type": "application/json",
      authorization:
        "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MzMsImlhdCI6MTcxODE2NjA5NiwiZXhwIjoxNzIwNzU4MDk2fQ.oWJefxnDGosktBlPQTvJ01Xqxa8YVAuhYs9MQPJE9po",
    },
    body: JSON.stringify({ data: { data } }),
  };

  return fetchClient(endpoint, options);
};

export const postAddComment = async (data: CommentRequest) => {
  const endpoint = `/comments`;
  const options = {
    method: "POST",
    headers: {
      "content-type": "application/json",
      authorization:
        "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MzMsImlhdCI6MTcxODE2NjA5NiwiZXhwIjoxNzIwNzU4MDk2fQ.oWJefxnDGosktBlPQTvJ01Xqxa8YVAuhYs9MQPJE9po",
    },
    body: JSON.stringify({ data: { data } }),
  };

  return fetchClient(endpoint, options);
};

async function fetchClient(
  endpoint: string,
  options: RequestInit
): Promise<any> {
  try {
    const res = await fetch(`${environments.pdfUrl}/api` + endpoint, options);
    const response = await res.json();

    if (res.ok) {
      return response;
    } else {
      throw new Error(response.error?.message || "Unknown error occurred");
    }
  } catch (error) {
    Logger.fail(`API request failed: ${error.message}`);
    throw error;
  }
}
