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
    welcome: {
      links: {
        learnHeader: "Learn",
        learnDescription: "Learn about the Tezos amendment process",
        discussHeader: "Discuss",
        discussDescription:
          "Join the global conversation about current and future proposals to amend Tezos",
        getStartedHeader: "Get Started",
        getStartedDescription:
          "Resources to store, transact, bake, and build with Tezoz",
      },
      currentPeriod: {
        header: "Current Period",
        periodType: {
          proposal: "Proposal",
          exploration: "Exploration",
          testing: "Testing",
          promotion: "Promotion",
        },
        remainingTime: "{{value, humanizeDurationFormat}} Remaining",
      },
    },
    proposals: {
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
        upvotesHeaderCaption: "Upvotes",
        hashCaption: "Hash: ",
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
    },
  },
};
