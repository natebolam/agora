export default {
  translation: {
    header: {
      logoCaption: "AGORA",
      wikiLink: "Tezos Wiki",
      getStartedLink: "Get Started",
      governanceLink: "Tezos Governance",
    },
    common: {
      discourseButton: "Discuss on Discourse",
      showMore: "Show more",
    },
    periodType: {
      proposal: "Proposal",
      exploration: "Exploration",
      testing: "Testing",
      promotion: "Promotion",
    },
    tezosLinks: {
      getStartedLink: "https://tezos.com/get-started",
      tezosWikiLink: "https://learn.tqtezos.com/",
      developersLink: "https://developers.tezos.com/",
      stackExchangeLink: "https://tezos.stackexchange.com/",
      blogLink: "https://medium.com/tezos",
      tezosGovernanceLink:
        "https://medium.com/tezos/amending-tezos-b77949d97e1e",
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
        timeAgo: "{{value, humanizeDateFormat}}",
        timeAgoCaption: "ago",
        filter: {
          buttonVotes: "{{percent}}% ({{total}})",
          inFavorCaption: "In Favor",
          againstCaption: "Against",
          passCaption: "Pass",
        },
        header: {
          baker: "Baker",
          votesAmount: "# of Votes",
          votesType: "Vote",
          hash: "Operation",
          date: "Date",
        },
      },
      timeTracker: {
        date: "{{- value, dateFormat}}",
      },
      participationTracker: {
        totalVotesValue: "{{value, numberFormat}}",
        totalVotes: "Votes Cast",
        participationValue: "{{value, numberFormat}}%",
        participation: "Participation",
        votesAvailableValue: "{{value, numberFormat}}",
        votesAvailable: "Votes Available",
      },
      proposalsList: {
        proposalsHeaderCaption: "Proposals",
        upvotesCaption: "Upvotes",
        upvotesValue: "{{value, numberFormat}}",
        hashCaption: "Hash: ",
        learnMore: "Learn more",
        discuss: "Discuss",
        noDescriptionCaption: "There is no description yet.",
      },
      proposalDescription: "Proposal Description",
      testingCountdown: {
        countdownCaption: "The Testing Period will be in progress from:",
        periodDate: "{{from, dateFormat}} - {{to, dateFormat}}",
        remainingTime: "{{value, humanizeDurationFormat}} Remaining",
      },
      periodSelect: {
        caption: "Period {{value}}",
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
    },
  },
};
