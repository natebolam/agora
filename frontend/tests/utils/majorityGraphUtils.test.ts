import { BallotsStats, VoteStats } from "../../src/models/Period";
import {
  ballotsMajorityColors,
  calculateBarsArray,
  calculateBarsWorkingWidth,
} from "../../src/utils/majorityGraphUtils";

describe("majority graph utils", (): void => {
  describe("calculateBarsWorkingWidth", (): void => {
    it("no votes", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 0,
        nay: 0,
        pass: 0,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 0,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(
        calculateBarsWorkingWidth(width, ballotsStats, voteStats)
      ).toStrictEqual({
        width: 600,
        votesLength: {
          yay: 0,
          nay: 0,
          pass: 0,
        },
      });
    });
    it("should reduce one bar", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 1199,
        nay: 0,
        pass: 1,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(
        calculateBarsWorkingWidth(width, ballotsStats, voteStats)
      ).toStrictEqual({
        width: 596,
        votesLength: {
          yay: 596,
          nay: 0,
          pass: 1,
        },
      });
    });
    it("should reduce two bars", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 1198,
        nay: 1,
        pass: 1,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(
        calculateBarsWorkingWidth(width, ballotsStats, voteStats)
      ).toStrictEqual({
        width: 592,
        votesLength: {
          yay: 592,
          nay: 1,
          pass: 1,
        },
      });
    });
    it("should reduce three bars", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 1,
        nay: 1,
        pass: 1,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(
        calculateBarsWorkingWidth(width, ballotsStats, voteStats)
      ).toStrictEqual({
        width: 0,
        votesLength: {
          yay: 1,
          nay: 1,
          pass: 1,
        },
      });
    });
    it("shouldn't reduce only spacer", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 400,
        nay: 400,
        pass: 400,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(
        calculateBarsWorkingWidth(width, ballotsStats, voteStats)
      ).toStrictEqual({
        width: 594,
        votesLength: {
          yay: 198,
          nay: 198,
          pass: 198,
        },
      });
    });

    it("shouldn't reduce width at all", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 1200,
        nay: 0,
        pass: 0,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(
        calculateBarsWorkingWidth(width, ballotsStats, voteStats)
      ).toStrictEqual({
        width: 600,
        votesLength: {
          yay: 600,
          nay: 0,
          pass: 0,
        },
      });
    });
  });

  describe("calculateBarsArray", (): void => {
    it("only yay", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 1200,
        nay: 0,
        pass: 0,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.yay,
          endX: 600,
          startX: 0,
        },
      ]);
    });
    it("only nay", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 0,
        nay: 1200,
        pass: 0,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.nay,
          endX: 600,
          startX: 0,
        },
      ]);
    });
    it("only pass", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 0,
        nay: 0,
        pass: 1200,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.pass,
          endX: 600,
          startX: 0,
        },
      ]);
    });

    it("tiny yay, tiny nay", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 1,
        nay: 1,
        pass: 0,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.yay,
          endX: 1,
          startX: 0,
        },
        {
          color: "white",
          endX: 4,
          startX: 1,
        },
        {
          color: ballotsMajorityColors.nay,
          endX: 5,
          startX: 4,
        },
      ]);
    });
    it("tiny yay, tiny pass", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 1,
        nay: 0,
        pass: 1,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.yay,
          endX: 1,
          startX: 0,
        },
        {
          color: "white",
          endX: 4,
          startX: 1,
        },
        {
          color: ballotsMajorityColors.pass,
          endX: 5,
          startX: 4,
        },
      ]);
    });
    it("tiny nay, tiny pass", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 0,
        nay: 1,
        pass: 1,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.nay,
          endX: 1,
          startX: 0,
        },
        {
          color: "white",
          endX: 4,
          startX: 1,
        },
        {
          color: ballotsMajorityColors.pass,
          endX: 5,
          startX: 4,
        },
      ]);
    });

    it("tiny nay, tiny pass", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 0,
        nay: 1,
        pass: 1,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.nay,
          endX: 1,
          startX: 0,
        },
        {
          color: "white",
          endX: 4,
          startX: 1,
        },
        {
          color: ballotsMajorityColors.pass,
          endX: 5,
          startX: 4,
        },
      ]);
    });
    it("equal all", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 400,
        nay: 400,
        pass: 400,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 1200,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual([
        {
          color: ballotsMajorityColors.yay,
          endX: 198,
          startX: 0,
        },
        {
          color: "white",
          endX: 201,
          startX: 198,
        },
        {
          color: ballotsMajorityColors.nay,
          endX: 399,
          startX: 201,
        },
        {
          color: "white",
          endX: 402,
          startX: 399,
        },
        {
          color: ballotsMajorityColors.pass,
          endX: 600,
          startX: 402,
        },
      ]);
    });
    it("no votes", (): void => {
      const width = 600;
      const ballotsStats: BallotsStats = {
        yay: 0,
        nay: 0,
        pass: 0,
        quorum: 0,
        supermajority: 0,
      };
      const voteStats: VoteStats = {
        votesAvailable: 0,
        votesCast: ballotsStats.yay + ballotsStats.nay + ballotsStats.pass,
      };
      expect(calculateBarsArray(width, ballotsStats, voteStats)).toStrictEqual(
        []
      );
    });
  });
});
