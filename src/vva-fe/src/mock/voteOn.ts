export const VOTE_ON_DATA = [
  {
    title: "No Confidence",
    actions: [
      {
        id: "123456",
        type: "for example",
        vote: "yes",
        expiryDate: "1970-01-01",
      },
      {
        id: "65432",
        type: "gov action",
        vote: "no",
        expiryDate: "1970-01-01",
      },
    ],
  },
  {
    title: "New constitutional committee or quorum size",
    actions: [
      {
        id: "abc123",
        type: "action gov",
        vote: "abstain",
        expiryDate: "1970-01-01",
      },
      {
        id: "123cba",
        type: "voting action",
        vote: "yes",
        expiryDate: "1970-01-01",
      },
    ],
  },
] as VotedOnDataType;
