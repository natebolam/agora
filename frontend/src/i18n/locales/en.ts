export default {
  translation: {
    header: {
      logoCaption: "AGORA",
      wikiLink: "Wiki",
      getStartedLink: "Get Started",
      governanceLink: "Learn",
      forumLink: "Forum",
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
    stageType: {
      proposal: "Proposal",
      implementation: "Implementation",
      voting: "Voting",
      evaluation: "Evaluation",
    },
    tezosLinks: {
      learnLink: "/learn",
      getStartedLink: "https://tezos.com/get-started",
      tezosWikiLink: "https://learn.tqtezos.com/",
      developersLink: "https://developers.tezos.com/",
      stackExchangeLink: "https://tezos.stackexchange.com/",
      blogLink: "https://medium.com/tezos",
      stakerDAOGovernanceLink: "https://www.stakerdao.com/",
      stakerDAOForumLink: "https://forum.stakerdao.com/",
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
      currentStage: {
        header: "Current Stage",
        remainingTime: "{{value, humanizeDurationFormat}} Remaining",
      },
    },
    proposals: {
      common: {
        noDescriptionCaption: "There is no description yet.",
        noProposalsCaption: "Nothing has been proposed at this stage",
      },
      details: {
        timeCaption: "{{value, dateFormat}}",
        timeTitle: "Time",
        hashTitle: "Hash",
        urlsTitle: "URLs",
        proposerTitle: "Proposer",
        proposalFileTitle: "Proposal File",
      },
      votersTable: {
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
          voter: "Voter",
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
        numVoters: "Voters",
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
        countdownCaption: "The Testing Stage will be in progress from:",
        countdownFinishedCaption:
          "The Testing Stage has been in progress from:",
        stageDate: "{{from, dateFormat}} - {{to, dateFormat}}",
        remainingTime: "{{value, humanizeDurationFormat}} Remaining",
        remainingTimeFinished: "Testing stage has been finished",
      },
      stageSelect: {
        caption: "{{value}}",
        captionDate:
          "{{value}}. {{stageType}} ({{startTime, dateFormat}} - {{endTime, dateFormat}})",
        captionDateMobile:
          "{{value}} ({{startTime, dateFormat}} - {{endTime, dateFormat}})",
      },
      recentVotes: {
        header: {
          recentVotesCaption: "Recent Votes",
          timeCaption: "Time",
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
