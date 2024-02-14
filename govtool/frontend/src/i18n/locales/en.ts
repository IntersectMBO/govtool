export const en = {
  translation: {
    alerts: {
      delegation: {
        failed: "Delegation transaction failed",
        refreshPage:
          "Your voting power has been successfully delegated! Please refresh the page.",
        success: "Your voting power has been successfully delegated!",
      },
      metadataUpdate: {
        failed: "Update DRep metadata transaction failed",
        success: "You have successfully updated DRep metadata!",
      },
      registration: {
        failed: "Registration transaction failed",
        refreshPage:
          "You have successfully registered as a DRep! Please refresh the page.",
        success: "You have successfully registered as a DRep!",
      },
      retirement: {
        failed: "Retirement transaction failed",
        refreshPage:
          "You have successfully retired from being a DRep! Please refresh the page.",
        success: "You have successfully retired from being a DRep!",
      },
      voting: {
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
      delegation: {
        changeDelegation: "Change delegation",
        connectToDelegate: "Connect to delegate",
        delegateOwnPower:
          "If you want to delegate your own voting power of ₳<strong>{{ada}}</strong>.",
        description:
          "If you want to delegate to a DRep or select a default option.",
        dRepDelegatedTo: "DRep you delegated to",
        toDRep:
          "You have delegated your voting power of ₳<strong>{{ada}}</strong> to a selected DRep.",
        toYourself:
          "You have delegated your voting power of ₳<strong>{{ada}}</strong> to yourself.",
        useYourVotingPower: "Use your Voting Power",
        voteAbstain:
          "You have delegated your voting power of ₳<strong>{{ada}}</strong>. You are going to vote 'ABSTAIN' as default.",
        voteNo:
          "You have delegated your voting power of ₳<strong>{{ada}}</strong>. You are going to vote 'NO' as default.",
        votingPowerDelegation: "Voting Power Delegation",
        yourVotingPowerIsDelegated:
          "Your Voting Power <strong>is Delegated</strong>",
        inProgress: {
          toDRep:
            "Your own voting power of ₳<strong>{{ada}}</strong> is progress of being delegated. You are going to delegate your voting power to a selected DRep.",
          toYourself:
            "Your own voting power of ₳<strong>{{ada}}</strong> is in progress of being delegated. You are going to delegate your voting power to yourself.",
          voteAbstain:
            "Your own voting power of ₳<strong>{{ada}}</strong> is in progress of being delegated. You are going to vote ‘ABSTAIN’ as default.",
          voteNo:
            "Your own voting power of ₳<strong>{{ada}}</strong> is in progress of being delegated. You are going to vote ‘NO’ as default.",
        },
      },
      govActions: {
        description: "Review governance actions submitted on-chain.",
        reviewAndVote: "Review and vote",
        title: "Governance Actions",
        view: "View governance actions",
      },
      registration: {
        changeMetadata: "Change metadata",
        connectToRegister: "Connect to register",
        dRepRegistration: "DRep Registration",
        dRepRetirement: "DRep Retirement",
        dRepUpdate: "DRep Update",
        description:
          "If you want to directly participate in voting and have other ada holders delegate their voting power to you.",
        holdersCanDelegate:
          "Ada holders can delegate their voting power to you.",
        ifYouWant:
          "If you want to directly participate in voting and have other ada holders delegate their voting power to you.",
        metadataUpdateInProgress:
          "The update DRep metadata is ongoing. This may take several minutes.",
        register: "Register",
        registerAgain: "Register Again as a dRep",
        registerAsDRep: "Register as a DRep",
        registrationInProgress:
          "The registration process is ongoing. This may take several minutes.",
        retire: "Retire as a DRep",
        retirementInProgress:
          "The retirement process is ongoing. This may take several minutes.",
        youAreRegistered: "You are Registered as a DRep",
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
    errorPage: {
      backToDashboard: "Back to dashboard",
      backToHomepage: "Back to homepage",
      error: "Error ",
      serverError: "We have an internal server error.",
      whoops: "Whoops!",
    },
    errors: {
      appCannotCreateTransaction: "Application can not create transaction.",
      appCannotGetUtxos: "Application can not get utxos",
      checkIsWalletConnected: "Check if the wallet is connected.",
      dRepIdNotFound: "DrepId not found",
      notUsingAnchor: "DRep Registration - not using anchor",
      noAddressesFound: "No addresses found",
      noStakeKeySelected: "No stake key selected",
      registeringStakeKey: "Registering stake key",
      somethingWentWrong: "Something went wrong",
      useCardano: "useCardano must be used within a CardanoProvider",
      tryingConnectTo:
        "You are trying to connect with a wallet connected to {{networkFrom}}. Please adjust your wallet settings to connect to {{networkTo}} or select a different wallet.",
      walletNoCIP30Nor90Support:
        "Your wallet does not support the required CIP-30 extension, CIP-95.",
      walletNoCIP30Support: "Your wallet does not support CIP-30 extensions.",
      walletNoCIP90FunctionsEnabled:
        "Your wallet did not enable the needed CIP-95 functions during connection.",
    },
    footer: {
      copyright: "© 2023 Voltaire Gov Tool",
      privacyPolicy: "Privacy policy",
    },
    forms: {
      hashPlaceholder: "The hash of your URL",
      howCreateUrlAndHash: "How to create URL and hash?",
      urlWithContextPlaceholder: "Your URL with with your context",
      urlWithInfoPlaceholder: "Your URL with extra info about you",
      errors: {
        hashInvalidFormat: "Invalid hash format",
        hashInvalidLength: "Hash must be exactly 64 characters long",
        urlTooLong: "Url must be less than 65 characters",
        urlInvalidFormat: "Invalid URL format",
      },
    },
    govActions: {
      changeVote: "Change vote",
      changeYourVote: "Change your vote",
      chooseHowToVote: "Choose how you want to vote:",
      details: "Governance Details:",
      expiryDate: "Expiry date:",
      filterTitle: "Governance Action Type",
      forGovAction: "for this Governance Action",
      governanceActionId: "Governance Action ID:",
      governanceActionType: "Governance Action Type:",
      myVote: "My Vote:",
      noResultsForTheSearch: "No results for the search.",
      optional: "(optional)",
      provideContext: "Provide context about your vote",
      selectDifferentOption: "Select a different option to change your vote",
      showVotes: "Show votes",
      submissionDate: "Submission date:",
      title: "Governance Actions",
      toVote: "To vote",
      viewOtherDetails: "View other details",
      viewProposalDetails: "View proposal details",
      vote: "Vote",
      voted: "Voted",
      voteOnGovActions: "Vote on Governance Action",
      voteSubmitted: "Vote submitted",
      voteTransaction: "Vote transaction",
      votes: "Votes:",
      votesSubmitted: "Votes submitted",
      votesSubmittedOnChain:
        "Votes submitted on-chain by DReps, SPOs and Constitutional Committee members.",
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
    },
    hero: {
      connectWallet: "Connect your wallet",
      description:
        "Interact with SanchoNet using GovTool - a friendly user\ninterface connected to SanchoNet. You can delegate\nyour voting power (tAda) or become a SanchoNet DRep\nto allow people to delegate voting power to you.",
      headline: "SanchoNet \n Governance Tool",
    },
    menu: {
      faqs: "FAQs",
      guides: "Guides",
      help: "Help",
      myDashboard: "My Dashboard",
      viewGovActions: "View Governance Actions",
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
      delegation: {
        message:
          "The confirmation of your actual delegation might take a bit of time but you can track it using",
        title: "Delegation Transaction Submitted!",
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
        message:
          "The confirmation of your registration might take a bit of time but you can track it using",
        title: "Registration Transaction Submitted!",
      },
      retirement: {
        message:
          "The confirmation of your retirement might take a bit of time but you can track it using",
        title: "Retirement Transaction Submitted!",
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
    },
    registration: {
      descriptionStepOne:
        "You can include extra information about yourself by adding a URL and its hash.",
      descriptionStepTwo:
        "By clicking register you create your DRep ID within your wallet and become a DRep.\n\nOnce the registration has completed your DRep ID will be shown on your dashboard. You will be able to share your DRep ID so that other ada holders can delegate their voting power to you.",
      headingStepOne: "Add Information",
      headingStepTwo: "Confirm DRep registration",
      optional: "OPTIONAL",
      register: "Register",
      registerAsDRep: "Register as a DRep",
    },
    slider: {
      showAll: "Show all",
      viewAll: "View all",
    },
    soleVoter: {
      becomeSoleVoter: "Become a Sole Voter",
      continueToRegister: "Continue to register",
      description:
        "A Sole Voter is someone that can vote on any Governance Action with their own Voting Power, which is equal to the balance of ADA in their connected wallet. <0>Learn More</0> about Sole Voter.\n\nBecoming a Sole Voter will require a refundable deposit of <strong>₳2</strong>.\n\nYour deposit will be refunded if you either retire or delegate your voting power to someone else (a DRep)",
      heading: "What this Means",
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
      connectWallet: "Connect wallet",
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
    abstain: "Abstain",
    back: "Back",
    backToList: "Back to the list",
    backToDashboard: "Back to dashboard",
    cancel: "Cancel",
    clear: "Clear",
    confirm: "Confirm",
    continue: "Continue",
    delegate: "Delegate",
    here: "here",
    inProgress: "In progress",
    learnMore: "Learn more",
    loading: "Loading...",
    myDRepId: "My DRep ID:",
    nextStep: "Next step",
    no: "No",
    ok: "Ok",
    select: "Select",
    seeTransaction: "See transaction",
    skip: "Skip",
    sortBy: "Sort by",
    thisLink: "this link",
    votingPower: "Voting power:",
    yes: "Yes",
  },
};
