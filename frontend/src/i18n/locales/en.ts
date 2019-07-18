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
    proposals: {
      bakersTable: {
        upvoteTrue: "yay",
        upvoteFalse: "nah",
        timeAgo: "{{value, humanizeDateFormat}} ago",
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
        remainingTime: "{{value, humanizeDateFormat}} Remaining",
      },
    },
  },
};
