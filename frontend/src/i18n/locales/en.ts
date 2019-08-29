export default {
  translation: {
    header: {
      logoCaption: "AGORA",
      wikiLink: "Wiki",
      getStartedLink: "Get Started",
      governanceLink: "Tezos Governance",
    },
    common: {
      discourseButton: "Discuss on Discourse",
      discourseButtonMobile: "Discourse",
      showAll: "Show all",
      learnMoreButton: "Learn More",
    },
    errorPage: {
      errorCodeCaption: "Error code:",
      homeButtonCaption: "Home",
      errors: {
        "404": {
          errorCaption: "Page not found",
          errorDescription:
            "The requested page cannot be found. It may have been removed or " +
            "the link can be broken. If you entered a web address please check " +
            "it was correct and try again. ",
        },
        "500": {
          errorCaption: "Internal server error",
          errorDescription:
            "Something went wrong on the server side. Try to refresh the web page" +
            " a little bit later.",
        },
      },
    },
    learnPage: {
      title: "Tezos Governance Overview",
    },
    periodType: {
      proposal: "Proposal",
      exploration: "Exploration",
      testing: "Testing",
      promotion: "Promotion",
    },
    tezosLinks: {
      learnLink: "/learn",
      getStartedLink: "https://tezos.com/get-started",
      tezosWikiLink: "https://learn.tqtezos.com/",
      developersLink: "https://developers.tezos.com/",
      stackExchangeLink: "https://tezos.stackexchange.com/",
      blogLink: "https://medium.com/tezos",
      tezosGovernanceLink: "/learn",
    },
    welcome: {
      links: {
        learnHeader: "Learn",
        learnDescription: "Learn about the Tezos amendment process",
        discussHeader: "Discuss",
        discussDescription:
          "Join the global conversation about current and future proposals to amend Tezos",
        getStartedHeader: "Get Started",
        getStartedDescription:
          "Resources to store, transact, bake, and build with Tezos",
      },
      currentPeriod: {
        header: "Current Period",
        remainingTime: "{{value, humanizeDurationFormat}} Remaining",
      },
    },
    proposals: {
      common: {
        noDescriptionCaption: "There is no description yet.",
        noProposalsCaption: "Nothing has been proposed at this period",
      },
      details: {
        timeCaption: "{{value, dateFormat}}",
        timeTitle: "Time",
        hashTitle: "Hash",
        proposerTitle: "Proposer",
        proposalFileTitle: "Proposal File",
      },
      bakersTable: {
        voteYay: "Yay",
        voteNay: "Nay",
        votePass: "Pass",
        time: "{{value, dateFormat}}",
        timeAgo: "{{value, humanizeDateFormat}}",
        filter: {
          buttonVotes: "{{percent}}% ({{total, numberFormat}})",
          buttonPass: "{{total, numberFormat}}",
          inFavorCaption: "In Favor",
          againstCaption: "Against",
          passCaption: "Pass",
        },
        header: {
          baker: "Baker",
          votesAmount: "# of Votes",
          votesType: "Vote",
          hash: "Operation",
          time: "Time",
        },
      },
      timeTracker: {
        date: "{{- value, dateFormat}}",
      },
      participationTracker: {
        totalVotesValue:
          "{{value, numberFormat}} / {{available, numberFormat}}",
        totalVotes: "Votes Cast",
        totalVotesInverted: "Votes Available",
        participationValue: "{{value}}%",
        participation: "Participation",
        participationInverted: "Undecided",
        numVotersValue: "{{value, numberFormat}} / {{total, numberFormat}}",
        numVoters: "Bakers",
        numVotersInverted: "Non-voters",
      },
      proposalsList: {
        proposalsHeaderCaption: "Proposals",
        upvotesCaption: "Upvotes",
        upvotesValue: "{{value, numberFormat}}{{percent}}",
        upvotesPercentage: "{{value}}%",
        hashCaption: "Hash: ",
        learnMore: "Learn more",
        discuss: "Discuss",
        noDescriptionCaption: "There is no description yet.",
      },
      proposalDescription: "Proposal Description",
      testingCountdown: {
        countdownCaption: "The Testing Period will be in progress from:",
        countdownFinishedCaption:
          "The Testing Period has been in progress from:",
        periodDate: "{{from, dateFormat}} - {{to, dateFormat}}",
        remainingTime: "{{value, humanizeDurationFormat}} Remaining",
        remainingTimeFinished: "Testing period has been finished",
      },
      periodSelect: {
        caption: "{{value}}",
        captionDate:
          "{{value}}. {{periodType}} ({{startTime, dateFormat}} - {{endTime, dateFormat}})",
        captionDateMobile:
          "{{value}} ({{startTime, dateFormat}} - {{endTime, dateFormat}})",
      },
      recentVotes: {
        header: {
          recentVotesCaption: "Recent Votes",
          votesCaption: "Votes",
        },
        proposalHashCaption: "Proposal: ",
      },
      majorityGraph: {
        quorumCaption: "Quorum {{value}}%",
        supermajorityCaption: "Supermajority {{value}}%",
      },
      pieChart: {
        othersCaption: "Others",
      },
    },
  },
};
