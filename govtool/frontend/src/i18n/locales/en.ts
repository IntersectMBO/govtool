export const en = {
  translation: {
    alerts: {
      delegate: {
        failed: "Delegation transaction failed",
        warning:
          "Your voting power has been successfully delegated! Please refresh the page.",
        success: "Your voting power has been successfully delegated!",
      },
      createGovAction: {
        failed: "Creating Governance Action transaction failed",
        success: "Your Governance Action has been submitted",
      },
      registerAsDrep: {
        failed: "Registration transaction failed",
        warning:
          "You have successfully registered as a DRep! Please refresh the page.",
        success: "You have successfully registered as a DRep!",
      },
      registerAsDirectVoter: {
        failed: "Registration transaction failed",
        warning:
          "You have successfully registered as a Direct Voter! Please refresh the page.",
        success: "You have successfully registered as a Direct Voter!",
      },
      retireAsDrep: {
        failed: "Retirement transaction failed",
        warning:
          "You have successfully retired from being a DRep! Please refresh the page.",
        success: "You have successfully retired from being a DRep!",
      },
      retireAsDirectVoter: {
        failed: "Retirement transaction failed",
        warning:
          "You have successfully retired from being a Direct Voter! Please refresh the page.",
        success: "You have successfully retired from being a DirectVoter!",
      },
      updateMetaData: {
        failed: "Update DRep metadata transaction failed",
        success: "You have successfully updated DRep metadata!",
      },
      vote: {
        failed: "Vote transaction failed",
        success: "You have successfully voted!",
      },
      changesSaved: "Your changes have been saved",
      copiedToClipboard: "Copied to clipboard",
      transactionInProgress: "Transaction in progress. Please wait.",
      walletConnected: "Wallet connected",
    },
    dashboard: {
      headingOne: "Your Participation",
      headingTwo: "See Active Governance Actions",
      title: "Dashboard",
      cards: {
        drepName: "Drep_name",
        showTransaction: "Show Transaction",
        drep: {
          changeMetadata: "Change metadata",
          dRepRegistration: "DRep Registration",
          dRepRetirement: "DRep Retirement",
          dRepUpdate: "DRep Update",
          holdersCanDelegate:
            "Ada holders can delegate their voting power to you.",
          ifYouWant:
            "If you want to directly participate in voting and have other ada holders delegate their voting power to you.",
          metadataUpdateInProgress:
            "The update DRep metadata is ongoing. This may take several minutes.",
          notRegisteredDescription:
            "Accept delegated voting power from other ADA holders, and combine it with your own voting power. Vote with the accumulated Power on  Governance Actions.",
          notRegisteredTitle: "Become a DRep",
          notRegisteredWasRegisteredDescription:
            "You are now retired and your listing in the DRep directory as <strong>MrDRep</strong> has been marked as <strong><i>retired</i></strong>. You cannot vote on Governance Actions as <strong>MrDRep</strong>.\n\nYou can always re-register as a DRep, in which case you will have the same DRep ID as you had initially.",
          notRegisteredWasRegisteredTitle: "You Have Retired as a DRep.",
          register: "Register",
          registerAgain: "Register Again as a dRep",
          registerAsDRep: "Register as a DRep",
          registeredDescription:
            "You can Vote using your own Voting Power combined with any Voting Power delegated to you.",
          registeredTitle: "You are Registered as a DRep",
          registrationInProgress:
            "The registration process is ongoing. This may take several minutes.",
          reRegister: "Re-register as a DRep",
          retire: "Retire as a DRep",
          retirementInProgress:
            "You are being retired as <strong>MrDRep</strong>. You will receive a refund of <strong>{{deposit}} ADA</strong> when the transaction  completes.",
          viewDetails: "View your DRep details",
          youAreRegistered: "You are Registered as a DRep",
          yourDRepId: "Your DRep ID",
        },
        govActions: {
          description: "Review governance actions submitted on-chain.",
          reviewAndVote: "Review and vote",
          title: "View Governance Actions",
        },
        proposeGovernanceAction: {
          title: "Propose a Governance Action",
          description: "Submit your proposal for a Governance Action.",
          propose: "Propose",
          view: "View Governance Actions",
        },
        directVoter: {
          isRegisteredDescription:
            "Your Voting Power of ₳<strong>{{votingPower}}</strong> can be used to vote.",
          register: "Register",
          registerDescription:
            "Register to Vote on Governance Actions using your own voting power of ₳<strong>{{votingPower}}</strong>.",
          registerTitle: "Become a Direct Voter",
          reRegister: "Re-register",
          registration: "Direct Voter Registration",
          registrationInProgress:
            "The registration process is ongoing. This may take several minutes.",
          retire: "Retire",
          wasDirectVoterTitle: "You Have Retired as a Direct Voter",
          retirement: "Direct Voter Retirement",
          retirementInProgress:
            "The retirement process is ongoing. This may take several minutes.",
          wasRegisteredDescription:
            "You cannot vote on Governance Actions using your own voting power of ₳<strong>{{votingPower}}</strong>. until you re-register.",
          youAreDirectVoterTitle: "You are a Direct Voter",
        },
        delegation: {
          noDelegationTitle: "Delegate your Voting Power",
          delegateToAnotherDRep: "Delegate to Another DRep",
          noDelegationDescription: "Find a DRep to vote on your behalf.",
          noDelegationActionButton: "View DRep Directory",
          dRepDelegationTitle:
            "Your Voting Power of <strong>₳{{ada}}</strong>\nis Delegated to:",
          noConfidenceDelegationTitle:
            "You have delegated <strong>₳{{ada}}</strong>\nto “No Confidence”",
          abstainDelegationTitle:
            "You have delegated <strong>₳{{ada}}</strong>\nto “Abstain”",
          abstainDescription:
            "You have selected to apply your Voting Power to Abstain on every vote.",
          noDescription:
            "You have selected to apply your Voting Power to No Confidence on every vote.",
          inProgress: {
            title: "Delegation",
            dRep: "Your voting power of ₳<strong>{{ada}}</strong> is being delegated to:",
            abstain:
              "You have selected to apply your Voting Power to Abstain on every vote.",
            no: "You have selected to apply your Voting Power to No Confidence on every vote.",
          },
        },
      },
    },
    createGovernanceAction: {
      chooseGATypeTitle: "Choose a Governance Action type",
      creatingAGovernanceAction:
        "Creating a Governance Action: What you need to know",
      creatingAGovernanceActionDescription:
        "To create a Governance Action, you will need to:\n\n• Fill out a form with the relevant data\n• Pay a refundable deposit of <strong>₳{{deposit}}</strong>\n• Store the metadata of your Governance Action at your own expense.\n\nYour deposit will be refunded to your wallet when the Governance Action is either enacted or expired.\n\nThe deposit will not affect your Voting Power.",
      editSubmission: "Edit submission",
      fields: {
        declarations: {
          abstract: {
            label: "Abstract",
            placeholder: "Summary",
            tip: "General summary of the Action",
          },
          amount: {
            label: "Amount",
            placeholder: "e.g. 20000",
          },
          motivation: {
            label: "Motivation",
            placeholder: "Problem this GA will solve",
            tip: "How will this solve a problem",
          },
          rationale: {
            label: "Rationale",
            placeholder: "Content of Governance Action",
            tip: "Put all the content of the GA here",
          },
          receivingAddress: {
            label: "Receiving Address",
            placeholder: "The address to receive funds",
          },
          title: {
            label: "Title",
            placeholder: "A name for this Action",
          },
        },
        validations: {
          bech32: "Invalid bech32 address",
          maxLength: "Max {{maxLength}} characters",
          number: "Only number is allowed",
          required: "This field is required",
          url: "Invalid URL",
          positive: "Only positive number is allowed",
        },
      },
      formTitle: "Governance Action details",
      references: "References and Supporting Information",
      reviewSubmission: "Review your submission",
      storeDataCheckboxLabel:
        "I agree to store correctly this information and to maintain them over the years",
      storeDataLink: "Learn more about storing information",
      storeDataTitle: "Store and Maintain the Data Yourself",
      storingInformationDescription:
        "Download your file, save it to your chosen location, and enter the URL of that location in step 3",
      storingInformationStep1Label: "Download this file",
      storingInformationStep2Label:
        "Save this file in a location that provides a public URL (ex. github)",
      storingInformationStep2Link: "Read full guide",
      storingInformationStep3Label: "Paste the URL here",
      storingInformationTitle: "Information Storage Steps",
      storingInformationURLPlaceholder: "URL",
      supportingLinks: "Supporting links",
      title: "Create a Governance Action",
      modals: {
        submitTransactionSuccess: {
          message:
            "Your Governance Action may take a little time to submit to the chain.",
          title: "Governance Action submitted!",
        },
      },
    },
    delegation: {
      description:
        "You can delegate your voting power to a DRep or to a pre-defined voting option.",
      dRepIdDescription: "The DRep ID is the identifier of a DRep.",
      heading: "Use your Voting Power",
      otherOptions: "Other options",
      pasteDRepId: "Paste DRep ID",
      votingPowerToDelegate: "Voting power to delegate:",
      whereFindDRepId: "The DRep ID is the identifier of a DRep.",
      abstain: {
        subtitle: "Select this to vote ABSTAIN to every vote.",
        title: "Vote ABSTAIN as default",
      },
      noConfidence: {
        subtitle:
          "Select this to signal no confidence in the current constitutional committee by voting NO on every proposal and voting YES to no-confidence proposals.",
        title: "Signal no confidence",
      },
      toDRep: {
        subtitle:
          "Select this to delegate to a DRep using their related DRep ID.",
        title: "Delegate to DRep",
      },
      toMyself: {
        subtitle: "Select this to delegate your own voting power to yourself.",
        title: "Delegate to myself",
      },
    },
    dRepDirectory: {
      abstainCardDefaultDescription:
        "Select this to vote ABSTAIN to every vote.",
      abstainCardDefaultTitle: "Abstain from Every Vote",
      automatedVotingOptions: "Automated Voting Options",
      editBtn: "Edit DRep data",
      delegatedToAbstainTitle: "You have delegated ₳{{ada}} to “Abstain”",
      delegatedToNoConfidenceTitle:
        "You have delegated ₳{{ada}} to “No Confidence”",
      delegatedToAbstainDescription:
        "You have selected to apply your Voting Power to Abstain on every vote.",
      delegatedToNoConfidenceDescription:
        "You have selected to apply your Voting Power to No Confidence on every vote.",
      delegationOptions: "Delegation Options",
      directVoter: "Direct Voter",
      filterTitle: "DRep Status",
      goToDRepDirectory: "Go to DRep Directory",
      meAsDRep: "This DRep ID is connected to your wallet",
      myDelegation: "You have delegated <strong>₳ {{ada}}</strong> to:",
      myDelegationToYourself:
        "You have delegated <strong>₳ {{ada}}</strong> to yourself:",
      myDRep: "You have delegated ₳{{ada}} to this DRep.",
      listTitle: "Find a DRep",
      noConfidenceDefaultDescription:
        "Select this to signal no confidence in the current constitutional committee by voting NO on every proposal and voting YES to no confidence proposals",
      noConfidenceDefaultTitle: "Signal No Confidence on Every Vote",
      noResultsForTheSearchTitle: "No DReps found",
      noResultsForTheSearchDescription: "Please try a different search",
      title: "DRep Directory",
      votingPower: "Voting Power",
    },
    errorPage: {
      backToDashboard: "Back to dashboard",
      backToHomepage: "Back to homepage",
      error: "Error ",
      serverError: "We have an internal server error.",
      whoops: "Whoops!",
    },
    errors: {
      appCannotCreateTransaction: "Application can not create transaction.",
      appCannotGetDeposit: "Can not fetch deposit",
      appCannotGetUtxos: "Application can not get utxos",
      appCannotGetVkeys: "Application can not get vkey",
      checkIsWalletConnected: "Check if the wallet is connected.",
      dRepIdNotFound: "DrepId not found",
      invalidGovernanceActionType: "Invalid Governance Action Type",
      invalidTreasuryGovernanceActionType: "Invalid Treasury Governance Action",
      noAddressesFound: "No addresses found",
      noStakeKeySelected: "No stake key selected",
      notUsingAnchor: "DRep Registration - not using anchor",
      registeringStakeKey: "Registering stake key",
      somethingWentWrong: "Something went wrong",
      tryingConnectTo:
        "You are trying to connect with a wallet connected to {{networkFrom}}. Please adjust your wallet settings to connect to {{networkTo}} or select a different wallet.",
      useCardano: "useCardano must be used within a CardanoProvider",
      walletNoCIP30Nor90Support:
        "Your wallet does not support the required CIP-30 extension, CIP-95.",
      walletNoCIP30Support: "Your wallet does not support CIP-30 extensions.",
      walletNoCIP90FunctionsEnabled:
        "Your wallet did not enable the needed CIP-95 functions during connection.",
      gAMetadata: {
        description: {
          notVerifiable:
            "GovTool uses external sources for Governance Action data, and these sources are maintained by the proposers of the Actions. This error means that the data stored by the Proposer does not match the data when the Governance Action was originally posted.",
          dataMissing:
            "GovTool uses external sources for Governance Action data, and these sources are maintained by the proposers of the Actions. This error means that GovTool cannot locate the data on the URL specified when the Governance Action was originally posted.",
          incorrectFormat:
            "GovTool uses external sources for Governance Action data, and these sources are maintained by the proposers of the Actions. This error means that the data stored by the Proposer does not match the data format as defined by the Cardano community.",
        },
        message: {
          notVerifiable:
            "The data that was originally used when this Governance Action was created has changed.",
          dataMissing:
            "The data that was originally used when this Governance Action was created has not been found.",
          incorrectFormat:
            "The data that was originally used when this Governance Action was created has been formatted incorrectly. ",
        },
        title: {},
      },
    },
    footer: {
      copyright: "© 2024 Intersect MBO",
      privacyPolicy: "Privacy policy",
      termOfService: "Term of service",
    },
    forms: {
      hashPlaceholder: "The hash of metadata at URL",
      howCreateUrlAndHash: "How to create URL and hash?",
      link: "Link",
      urlWithContextPlaceholder: "Your URL with with your context",
      urlWithInfoPlaceholder: "Your URL with extra info about you",
      createGovernanceAction: {
        typeLabel: "Governance Action Type",
        typeTip:
          "To change the Governance Action Type go back to the previous page.",
      },
      editMetadata: {
        bio: "Bio",
        bioHelpfulText: "Some sentence about yourself",
        bioPlaceholder: "Enter your Bio ...",
        dRepName: "DRep Name",
        dRepNameHelpfulText:
          "This is name that will be shown on your DRep profile",
        dRepNamePlaceholder: "ex. JohnDRep",
        email: "Email",
        emailPlaceholder: "john.smith@email.com",
      },
      errors: {
        hashInvalidFormat: "Invalid hash format",
        hashInvalidLength: "Hash must be exactly 64 characters long",
        urlTooLong: "Url must be less than 65 characters",
        urlInvalidFormat: "Invalid URL format",
      },
      registerAsDRep: {
        bio: "Bio",
        bioHelpfulText: "Some sentence about yourself",
        bioPlaceholder: "Enter your Bio ...",
        dRepName: "DRep Name",
        dRepNameHelpfulText:
          "This is name that will be shown on your DRep profile",
        dRepNamePlaceholder: "ex. JohnDRep",
        email: "Email",
        emailPlaceholder: "john.smith@email.com",
      },
    },
    govActions: {
      about: "About",
      abstract: "Abstract:",
      backToGovActions: "Back to Governance Actions",
      castVote:
        "<0>You voted {{vote}} on this proposal</0>\non {{date}} (Epoch {{epoch}})",
      castVoteDeadline:
        "You can change your vote up to {{date}} (Epoch {{epoch}})",
      changeVote: "Change vote",
      changeYourVote: "Change your vote",
      chooseHowToVote: "Choose how you want to vote:",
      contextAboutYourVote: "Context about your vote",
      dataMissing: "Data Missing",
      dataMissingTooltipExplanation:
        "Please click “View Details” for more information.",
      details: "Governance Details:",
      expiresDateWithEpoch: "Expires: <0>{{date}}</0> <1>(Epoch {{epoch}})</1>",
      expiryDate: "Expiry date:",
      filterTitle: "Governance Action Type",
      forGovAction: "for this Governance Action",
      governanceActionId: "Governance Action ID:",
      governanceActionType: "Governance Action Type:",
      goToVote: "Go to Vote",
      motivation: "Motivation",
      myVote: "My Vote:",
      noResultsForTheSearch: "No results for the search.",
      onChainTransactionDetails: "On-chain Transaction Details",
      optional: "(optional)",
      provideContext: "Provide context",
      provideContextAboutYourVote: "Provide context about your vote",
      provideNewContextAboutYourVote: "Provide new context about your vote",
      rationale: "Rationale",
      seeExternalData: "See external data",
      selectDifferentOption: "Select a different option to change your vote",
      showVotes: "Show votes",
      submissionDate: "Submission date:",
      submittedDateWithEpoch:
        "Submitted: <0>{{date}}</0> <1>(Epoch {{epoch}})</1>",
      supportingLinks: "Supporting links",
      title: "Governance Actions",
      toVote: "To vote",
      viewDetails: "View Details",
      viewOtherDetails: "View other details",
      viewProposalDetails: "View proposal details",
      vote: "Vote",
      voteContextJsonldFileName: "Vote_Context.jsonld",
      votedOnByMe: "Voted on by me",
      voteOnGovActions: "Vote on Governance Action",
      voteSubmitted: "Vote submitted",
      voteTransaction: "Vote transaction",
      votes: "Votes:",
      votesSubmitted: "Votes submitted",
      votesSubmittedOnChain:
        "Votes submitted on-chain by DReps, SPOs and Constitutional Committee members.",
      youCanProvideContext:
        "You can provide context about your vote. This information will be viewable by other users.",
      youHaventVotedYet:
        "You haven't voted on any Governance Actions yet. Check the 'To vote on' section to vote on Governance Actions.",
      withCategoryNotExist: {
        partOne: "Governnance actions with category",
        optional: "and search phrase",
        partTwo: "don't exist.",
      },
      withIdNotExist: {
        partOne: "Governance action with id",
        partTwo: "does not exist.",
      },
      tooltips: {
        info: "An action that has no effect on-chain, other than an on-chain record",
        treasury: "Withdrawals from the treasury",
      },
      type: {
        noConfidence: {
          title: "No Confidence",
          label: "No Confidence",
        },
        newCommittee: {
          title: "New Committee",
          label: "New Committee",
        },
        newConstitution: {
          title: "Update to the Constitution",
          label: "Update to the Constitution",
        },
        hardFork: {
          title: "Hard Fork",
          label: "Hard Fork",
        },
        parameterChange: {
          title: "Protocol Parameter Changes",
          label: "Protocol Parameter Change",
        },
        treasuryWithdrawals: {
          title: "Treasury Withdrawals",
          label: "Treasury Withdrawal",
        },
        infoAction: {
          title: "Info Action",
          label: "Info Action",
        },
      },
    },
    hero: {
      connectWallet: "Connect your wallet",
      description:
        "Anyone with a wallet containing ADA can participate in governance on Sanchonet.\n\nYour ADA balance entitles you to an equal amount of Voting Power.\n\nFor more info see the guide entry for <0>Voting Power</0>.",
      headline: "SanchoNet \n Governance Tool",
    },
    home: {
      cards: {
        delegate: {
          description: "Find a DRep to vote on your behalf.",
          firstButtonLabel: "View DRep Directory",
          title: "Delegate your Voting Power",
        },
        governanceActions: {
          description: "See all the Governance Actions submitted on chain. ",
          firstButtonLabel: "View Governance Actions",
          title: "View Governance Actions",
        },
        proposeAGovernanceAction: {
          description: "Submit your proposal for a Governance Action.",
          firstButtonLabel: "Connect to Submit",
          title: "Propose a Governance Action",
        },
        registerAsDRep: {
          description:
            "Accept delegated voting power from other ADA holders, and combine it with your own voting power. Vote with the accumulated Power on  Governance Actions.",
          firstButtonLabel: "Connect to Register",
          title: "Become a DRep",
        },
        registerAsDirectVoter: {
          description: "Vote on Governance Actions using your own voting power",
          firstButtonLabel: "Connect to Register",
          title: "Become a Direct Voter",
        },
      },
    },
    menu: {
      faqs: "FAQs",
      guides: "Guides",
      help: "Help",
    },
    metadataUpdate: {
      description:
        "You can include extra information about yourself by adding a URL and its hash.",
      info: "Update Information",
      title: "Update DRep Metadata",
    },
    modals: {
      common: {
        goToDashboard: "Go to Dashboard",
        oops: "Oops!",
      },
      createGovernanceAction: {
        cancelModalDescription:
          "Returning to the Dashboard will cancel your submission and your proposed Governance Action will not be submitted.",
        cancelModalTitle:
          "Do you want to Cancel your Governance Action submission?",
      },
      delegation: {
        message:
          "The confirmation of your actual delegation might take a bit of time but you can track it using",
        title: "Delegation Transaction Submitted!",
      },
      externalDataDoesntMatch: {
        buttonText: "Go to Data Edit Screen",
        cancelRegistrationText: "Cancel Registration",
        feedbackText: "Feedback",
        message:
          "GovTool checks the URL you entered to see if the JSON file that you self-host matches the one that was generated in GovTool. To complete registration, this match must be exact.\n\nIn this case, there is a mismatch. You can go back to the data edit screen and try the process again.",
        title: "Your External Data Does Not Match the Original File.",
      },
      externalLink: {
        beCareful: "Be Careful!",
        continueTo: "Continue to external link",
        description:
          "Exercise caution and verify the website's authenticity before sharing personal information. To proceed, click 'Continue'. To stay on Voltaire, click 'Cancel'.",
        safety: "External Link Safety",
        thisIs: "This is an external link:",
        youAreAboutToOpen: "You are about to open an external link to:",
      },
      registration: {
        cancelTitle: "Do You Want to Abandon Registration ?",
        cancelDescription:
          "If you return to the Dashboard, your information will not be saved.",
        message:
          "The confirmation of your registration might take a bit of time but you can track it using",
        title: "Registration Transaction Submitted!",
      },
      retirement: {
        message:
          "The confirmation of your retirement might take a bit of time but you can track it using",
        title: "Retirement Transaction Submitted!",
      },
      urlCannotBeFound: {
        buttonText: "Go to Data Edit Screen",
        cancelRegistrationText: "Cancel Registration",
        feedbackText: "Feedback",
        linkText: "Learn More about self-hosting",
        message:
          "GovTool cannot find the URL that you entered. Please check it and re-enter.",
        title: "The URL You Entered Cannot Be Found",
      },
      votingPower: {
        govActionsVotes: "Governance Action votes",
        votesSubmittedByDReps: "Votes submitted by DReps",
        yourVote: "Your vote",
      },
      waitForTransaction: {
        title: "Please wait for your previous transaction to be completed.",
        message:
          "Before performing a new action please wait for the previous action transaction to be completed.",
      },
      pendingValidation: {
        title: "GovTool Is Checking Your Data",
        message:
          "GovTool will read the URL that you supplied and make a check to see if it’s identical with the information that you entered on the form.",
      },
    },
    editMetadata: {
      pageTitle: "Edit DRep Info",
      aboutYou: "About You",
      aboutYouDescription:
        "Some extra info about you to provide context to delegators.",
      dRepName: "DRep Name",
      dRepNameDescription:
        "This is the name that will be displayed in the DRep Directory and it will be used also by delegators to find your profile.",
      linksDescription: "Links to extra content or social media contacts ",
      maximumLinks: "(maximum of {{numberOfLinks}} entries)",
      optional: "optional",
      required: "required",
      storeDataCheckboxLabel:
        "I agree to store correctly this information and to maintain them over the years",
      storeDataLink: "Learn more about storing information",
      storeDataTitle: "Store and maintain the data yourself",
      storingInformationDescription:
        "Download your file, save it to your chosen location, and enter the URL of that location in step 3",
      storingInformationStep1Label: "Download this file",
      storingInformationStep2Label:
        "Save this file in a location that provides a public URL (ex. github)",
      storingInformationStep2Link: "Read full guide",
      storingInformationStep3Label: "Paste the URL here",
      storingInformationTitle: "Information Storage Steps",
      storingInformationURLPlaceholder: "URL",
      fields: {
        validations: {
          email: "Invalid email address",
          maxLength: "Max {{maxLength}} characters",
          nickname: "Nickname can not contain whitespaces",
          required: "This field is required",
          url: "Invalid URL",
        },
      },
    },
    registration: {
      aboutYou: "About You",
      aboutYouDescription:
        "Some extra info about you to provide context to delegators.",
      addInformationDescription:
        "You can include extra information about yourself by adding a URL and its hash.",
      addInformationTitle: "Add Information",
      becomeADRep: "Become a DRep",
      descriptionStepTwo:
        "By clicking register you create your DRep ID within your wallet and become a DRep.\n\nOnce the registration has completed your DRep ID will be shown on your dashboard. You will be able to share your DRep ID so that other ada holders can delegate their voting power to you.",
      dRepName: "DRep Name",
      dRepNameDescription:
        "This is the name that will be displayed in the DRep Directory and it will be used also by delegators to find your profile.",
      headingStepTwo: "Confirm DRep registration",
      linksDescription: "Links to extra content or social media contacts ",
      maximumLinks: "(maximum of {{numberOfLinks}} entries)",
      optional: "optional",
      register: "Register",
      required: "required",
      rolesAndResponsibilitiesDescription:
        "DReps are fundamental users that govern the Cardano network. This is an important role which requires work and dedication to fulfil.\n\nA DRep is expected to actively participate in governance and act as a representative of other Cardano members in  governance matters. Therefore, DReps will be expected to keep abreast of Governance Actions so they can make informed and wise decisions.\n<0>Learn More</0> about DRep.\n\nPlease register as a DRep if you have time to dedicate to making Cardano a better and more well-governed place.\n\nBecoming a DRep will require a refundable deposit of ₳<strong>{{deposit}}</strong>.\n\nYou will be refunded your deposit when you retire.",
      rolesAndResponsibilitiesTitle: "Roles & Responsibilities",
      storeDataCheckboxLabel:
        "I agree to store correctly this information and to maintain them over the years",
      storeDataLink: "Learn more about storing information",
      storeDataTitle: "Store and maintain the data yourself",
      storingInformationDescription:
        "Download your file, save it to your chosen location, and enter the URL of that location in step 3",
      storingInformationStep1Label: "Download this file",
      storingInformationStep2Label:
        "Save this file in a location that provides a public URL (ex. github)",
      storingInformationStep2Link: "Read full guide",
      storingInformationStep3Label: "Paste the URL here",
      storingInformationTitle: "Information Storage Steps",
      storingInformationURLPlaceholder: "URL",
      fields: {
        validations: {
          email: "Invalid email address",
          maxLength: "Max {{maxLength}} characters",
          nickname: "Nickname can not contain whitespaces",
          required: "This field is required",
          url: "Invalid URL",
        },
      },
    },
    retirement: {
      continue: "Continue to Retirement",
      retireAsDrep: "Retire as a Drep",
      whatRetirementMeansTitle: "What Retirement Means",
      whatRetirementMeansDescription:
        "By retiring you are giving up your voting rights. Voting Power that is delegated to you will remain in place.\n\nADA Holders that have delegated to be able to see that you are retired in the DRep directory. They will be able to re-delegate their Voting Power to another DRep.\n\nYou can still participate in Governance by proposing Governance Actions, by delegating your personal Voting Power to another DRep, or by coming out of retirement, and assuming your previous role as a DRep.\n\nIf you come out of retirement, your DRep ID will be the same as it was before retirement, and your Voting Power will consist of your own ADA balance and what delegated power that remains associated\nto your DRep ID.",
    },
    slider: {
      showAll: "Show All",
      viewAll: "View all",
    },
    directVoter: {
      becomeDirectVoter: "Become a Direct Voter",
      registerDescription:
        "A Direct Voter is someone that can vote on any Governance Action with their own Voting Power, which is equal to the balance of ADA in their connected wallet. <0>Learn More</0> about Direct Voter.\n\nBecoming a Direct Voter will require a refundable deposit of <strong>₳{{deposit}}</strong>.\n\nYour deposit will be refunded if you either retire or delegate your voting power to someone else (a DRep)",
      registerHeading: "What this Means",
      retirementDescription:
        "By Retiring you are giving up your Voting Power. You will not be able to vote on any Governance Actions. Your deposit of {{deposit}} ADA will be refunded.\n\nYou can at any time in the future re-register to become a Direct Voter, or you can delegate your Voting Power to someone else, or become a DRep.\n\nThese options are listed in our Guides here: <0>Voting options and Roles</0>",
      retirementHeading: "What Retirement Means",
      retireDirectVoter: "Retire as a Direct Voter",
    },
    system: {
      sanchoNet: "SanchoNet",
      sanchoNetIsBeta:
        "The SanchoNet GovTool is currently in beta and it connects to ",
      testAdaNote:
        "Please note, this tool uses ‘Test ada’ <0>NOT real ada</0>. All governance actions and related terms pertain to SanchoNet.",
      toolConnectedToSanchonet: "This tool is connected to SanchoNet",
    },
    tooltips: {
      delegateTodRep: {
        abstain: {
          heading: "Abstaining",
          paragraphOne:
            "Select this to signal no confidence in the current constitutional committee by voting NO on every proposal and voting YES to no-confidence proposals.",
        },
        noConfidence: {
          heading: "No confidence",
          paragraphOne:
            "If you don’t have trust in the current constitutional committee you signal ‘No-confidence’. By voting ‘No’ means you don’t want governance actions to be ratified.",
        },
        todRep: {
          heading: "Delegation to DRep",
          paragraphOne:
            "DReps are representatives of the ada holders that can vote on governance actions.",
        },
        toMyself: {
          heading: "Delegate to myself",
          paragraphOne:
            "If you are registered as DRep you can delegate your voting power on yourself.",
        },
      },
      expiryDate: {
        heading: "Expiry Date",
        paragraphOne:
          "The date when the governance action will expiry if it doesn’t reach ratification thresholds.",
        paragraphTwo:
          "IMPORTANT: If the governance action is ratified before the expiry date it will be considered ratified and it will not be available to vote on afterwards.",
      },
      submissionDate: {
        heading: "Submission Date",
        paragraphOne:
          "The date when the governance action was submitted on-chain.",
      },
      votingPower: {
        heading: "DRep Voting Power",
        paragraphOne:
          "This is the voting power delegated to you as a DRep and it is calculated at the end of every epoch for the epoch that just ended.",
        paragraphTwo:
          "IMPORTANT: When voting, the voting power provides an indication and not the exact number.",
      },
    },
    wallet: {
      cantSeeWalletQuestion:
        "Can’t see your wallet? Check what wallets are currently compatible with GovTool ",
      chooseWallet: "Choose the wallet you want to connect with:",
      connect: "Connect",
      connectWallet: "Connect Wallet",
      connectYourWallet: "Connect your Wallet",
      connectYourWalletButton: "Connect your wallet",
      connectedWallet: "Connected Wallet:",
      disconnect: "Disconnect",
      noWalletsToConnect:
        "You don't have wallets to connect, install a wallet and refresh the page and try again",
      pickStakeKey: "Pick Stake Key",
      selectStakeKey: "Select the stake key you want to use:",
    },
    warnings: {
      usingUnregisteredStakeKeys:
        "Warning, no registered stake keys, using unregistered stake keys",
    },
    dataMissingErrors: {
      dataMissing: "Data Missing",
      notVerifiable: "Not Verifiable",
      incorrectFormat: "Data Formatted Incorrectly",
    },
    about: "About",
    abstain: "Abstain",
    addLink: "+ Add link",
    back: "Back",
    backToDashboard: "Back to dashboard",
    backToList: "Back to the list",
    cancel: "Cancel",
    clear: "Clear",
    clickToCopyLink: "Click to copy link",
    close: "Close",
    confirm: "Confirm",
    connectToDelegate: "Connect to delegate",
    continue: "Continue",
    copiedLink: "Copied link",
    delegate: "Delegate",
    drepId: "DRep ID",
    email: "Email",
    feedback: "Feedback",
    filter: "Filter",
    goBack: "Go back",
    here: "here",
    info: "Info",
    inProgress: "In Progress",
    learnMore: "Learn more",
    linkCopied: "Link copied",
    loading: "Loading...",
    moreInformation: "More information",
    myDRepId: "My DRep ID:",
    nextStep: "Next step",
    no: "No",
    ok: "Ok",
    optional: "Optional",
    register: "Register",
    required: "required",
    seeTransaction: "See transaction",
    select: "Select",
    share: "Share",
    showLess: "Show less",
    showMore: "Show more",
    skip: "Skip",
    sort: "Sort",
    sortBy: "Sort by",
    status: "Status",
    submit: "Submit",
    thisLink: "this link",
    viewDetails: "View details",
    votingPower: "Voting power",
    yes: "Yes",
    yourself: "Yourself",
  },
};
