export const GOVERNANCE_ACTION_TYPES = ["Info", "Treasury"];

export const GOVERNANCE_ACTIONS_FIELDS = [
  {
    name: "Info",
    fields: {
      title: {
        component: "input",
        placeholder: "A name for this Action",
        tip: "",
      },
      abstract: {
        component: "textarea",
        placeholder: "Summary",
        tip: "General summary of the Action",
      },
      motivation: {
        component: "textarea",
        placeholder: "Problem this GA will solve",
        tip: "How will this solve a problem",
      },
      rationale: {
        component: "textarea",
        placeholder: "Content of Governance Action",
        tip: "Put all the content of the GA here",
      },
    },
  },
  {
    name: "Treasury",
    fields: {
      title: {
        component: "input",
        placeholder: "A name for this Action",
        tip: "",
      },
      abstract: {
        component: "textarea",
        placeholder: "Summary",
        tip: "General summary of the Action",
      },
      motivation: {
        component: "textarea",
        placeholder: "Problem this GA will solve",
        tip: "How will this solve a problem",
      },
      rationale: {
        component: "textarea",
        placeholder: "Content of Governance Action",
        tip: "Put all the content of the GA here",
      },
      receiving_address: {
        component: "input",
        placeholder: "Receiving address",
        tip: "",
      },
      amount: {
        component: "input",
        placeholder: "e.g. 20000",
        tip: "",
      },
    },
  },
];
