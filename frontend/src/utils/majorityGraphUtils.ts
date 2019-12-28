import { BallotsStats, VoteStats } from "~/models/Stage";

export const ballotsMajorityColors = {
  yay: "#2eca9b",
  nay: "#fd2d49",
  pass: "#8ab7fa",
};

export interface BarInfo {
  startX: number;
  endX: number;
  color: string;
}

export const calculateBarsWorkingWidth = (
  width: number,
  ballotsStats: BallotsStats,
  voteStats: VoteStats
): {
  width: number;
  votesLength: {
    yay: number;
    nay: number;
    pass: number;
  };
} => {
  if (voteStats.votesAvailable === 0 || voteStats.votesAvailable === 0) {
    return {
      width,
      votesLength: {
        yay: 0,
        nay: 0,
        pass: 0,
      },
    };
  }
  const votesRatio = voteStats.votesCast / voteStats.votesAvailable;
  let resultWidth: number = Math.max(width * votesRatio, 9);
  let resultVotes = voteStats.votesCast;
  if (
    ballotsStats.yay > 0 &&
    (width * ballotsStats.yay) / voteStats.votesAvailable < 12
  ) {
    resultWidth--;
    resultVotes -= ballotsStats.yay;
  }
  if (
    ballotsStats.nay > 0 &&
    (width * ballotsStats.nay) / voteStats.votesAvailable < 1
  ) {
    resultWidth--;
    resultVotes -= ballotsStats.nay;
  }
  if (
    ballotsStats.pass > 0 &&
    (width * ballotsStats.pass) / voteStats.votesAvailable < 1
  ) {
    resultWidth--;
    resultVotes -= ballotsStats.pass;
  }
  if (ballotsStats.yay > 0 && (ballotsStats.nay > 0 || ballotsStats.pass > 0)) {
    resultWidth -= 3;
  }
  if (ballotsStats.nay > 0 && ballotsStats.pass > 0) {
    resultWidth -= 3;
  }
  if (resultVotes === 0) {
    return {
      width: resultWidth,
      votesLength: {
        yay: 1,
        nay: 1,
        pass: 1,
      },
    };
  }
  return {
    width: resultWidth,
    votesLength: {
      yay:
        ballotsStats.yay > 0
          ? Math.max(1, (resultWidth * ballotsStats.yay) / resultVotes)
          : 0,
      nay:
        ballotsStats.nay > 0
          ? Math.max(1, (resultWidth * ballotsStats.nay) / resultVotes)
          : 0,
      pass:
        ballotsStats.pass > 0
          ? Math.max(1, (resultWidth * ballotsStats.pass) / resultVotes)
          : 0,
    },
  };
};

export const calculateBarsArray = (
  barWidth: number,
  ballotsStats: BallotsStats,
  voteStats: VoteStats
): BarInfo[] => {
  const result: BarInfo[] = [];

  const { votesLength } = calculateBarsWorkingWidth(
    barWidth,
    ballotsStats,
    voteStats
  );
  let currentX = 0;
  if (ballotsStats.yay > 0) {
    const startX = currentX;
    const endX = startX + votesLength.yay;
    currentX = endX;
    result.push({
      startX,
      endX,
      color: ballotsMajorityColors.yay,
    });
  }
  if (ballotsStats.yay > 0 && (ballotsStats.nay > 0 || ballotsStats.pass > 0)) {
    const startX = currentX;
    const endX = startX + 3;
    currentX = endX;
    result.push({
      startX,
      endX,
      color: "white",
    });
  }
  if (ballotsStats.nay > 0) {
    const startX = currentX;
    const endX = startX + votesLength.nay;
    currentX = endX;
    result.push({
      startX,
      endX,
      color: ballotsMajorityColors.nay,
    });
  }
  if (ballotsStats.nay > 0 && ballotsStats.pass > 0) {
    const startX = currentX;
    const endX = startX + 3;
    currentX = endX;
    result.push({
      startX,
      endX,
      color: "white",
    });
  }
  if (ballotsStats.pass > 0) {
    const startX = currentX;
    const endX = startX + votesLength.pass;
    currentX = endX;
    result.push({
      startX,
      endX,
      color: ballotsMajorityColors.pass,
    });
  }

  return result;
};
