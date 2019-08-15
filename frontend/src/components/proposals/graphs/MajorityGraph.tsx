import React, {
  FunctionComponent,
  ReactElement,
  useEffect,
  useRef,
  useState,
} from "react";
import cx from "classnames";
import ReactResizeDetector from "react-resize-detector";
import * as d3 from "d3";
import { BallotsStats, VoteStats } from "~/models/Period";
import { BarInfo, calculateBarsArray } from "~/utils/majorityGraphUtils";
import styles from "~/styles/components/proposals/graphs/MajorityGraph.scss";
import { useTranslation } from "react-i18next";

const grpahBackgroundColor = "rgba(44, 125, 247, 0.1)";
const markColor = "#123262";
const captionDistance = 8;
const barHeight = 16;
const animationDuration = 600;

type MajorityGraphType = "quorum" | "majority";
type UpdateGraphCallback = (width: number, newBarArray: BarInfo[]) => void;

interface GraphTypes {
  width: number;
  ballotsStats: BallotsStats;
  voteStats: VoteStats;
  type: MajorityGraphType;
  markCaption: string;
  markValue: number;
}

const drawGraph = (
  graph: SVGSVGElement,
  graphSettings: GraphTypes
): ((width: number, arg: BarInfo[]) => void)[] => {
  const {
    width,
    ballotsStats,
    voteStats,
    type,
    markCaption,
    markValue,
  } = graphSettings;

  const barArray = calculateBarsArray(width, ballotsStats, voteStats);

  const svg = d3
    .select(graph)
    .attr("height", 48)
    .attr("width", "100%");

  const graphWrapper = svg.append("svg");
  const graphBar = graphWrapper.append("svg").attr("y", "32");
  const graphBarMain = graphBar.append("svg");
  const updateFunctions: UpdateGraphCallback[] = [];

  // Main clipping object for everything on bar including background.
  const mainClipPath = graphBar
    .append("clipPath")
    .attr("id", `bar-${type}`)
    .append("rect")
    .attr("height", barHeight)
    .attr("width", width)
    .attr("rx", barHeight / 2);

  // Creating background
  const backgroundBar = graphBarMain
    .append("rect")
    .attr("height", barHeight)
    .attr("x", 0)
    .attr("width", width)
    .attr("fill", grpahBackgroundColor)
    .attr("clip-path", `url(#bar-${type})`);

  // Add clip path for yay, nay, and pass
  const barClipPath = graphBarMain
    .append("clipPath")
    .attr("id", `mainBar-${type}`)
    .append("rect")
    .attr("height", barHeight)
    .attr("width", barArray.length > 0 ? barArray[barArray.length - 1].endX : 0)
    .attr("rx", barHeight / 2)
    .attr("clip-path", `url(#bar-${type})`);

  // Draw yay, nay, pass, and white dividers
  barArray.forEach((item, index): void => {
    const rect = graphBarMain
      .append("rect")
      .attr("height", barHeight)
      .attr("fill", item.color);
    rect
      .transition()
      .duration(animationDuration)
      .attr("x", item.startX)
      .attr("width", item.endX - item.startX);
    if (item.color !== "white") {
      rect.attr("clip-path", `url(#mainBar-${type})`);
    }

    // Add function to update svg on resize
    updateFunctions.push((width: number, newBarArray: BarInfo[]): void => {
      rect
        .attr("x", newBarArray[index].startX)
        .attr("width", newBarArray[index].endX - newBarArray[index].startX);
    });
  });

  // Mark line with caption
  const markLine = graphWrapper
    .append("line")
    .style("stroke", markColor)
    .style("stroke-width", 2)
    .style("opacity", 0.1)
    .attr("x1", (width * markValue) / 100)
    .attr("y1", 0)
    .attr("x2", (width * markValue) / 100)
    .attr("y2", 20);
  const markText = graphWrapper
    .append("text")
    .style("color", markColor)
    .style("opacity", 0.5)
    .style("font-size", 12)
    .style("font-weight", 500)
    .attr("height", 20)
    .text(markCaption)
    .attr("dominant-baseline", "hanging")
    .attr("y", 5)
    .attr("text-anchor", function(): string {
      const textWidth = this.getComputedTextLength();
      if (textWidth > (width * (100 - markValue)) / 100 - captionDistance) {
        return "end";
      }
      return "start";
    })
    .attr("x", function(): number {
      const textWidth = this.getComputedTextLength();
      if (textWidth > (width * (100 - markValue)) / 100 - captionDistance) {
        return (width * markValue) / 100 - captionDistance;
      }
      return (width * markValue) / 100 + captionDistance;
    });

  updateFunctions.push((width: number, newBarArray: BarInfo[]): void => {
    mainClipPath.attr("width", width);
    backgroundBar.attr("width", width);
    barClipPath.attr(
      "width",
      newBarArray.length > 0 ? newBarArray[newBarArray.length - 1].endX : 0
    );
    markLine
      .attr("x1", (width * markValue) / 100)
      .attr("x2", (width * markValue) / 100);

    // Drawing caption to left or to right of mark line.
    markText
      .attr("text-anchor", function(): string {
        const textWidth = this.getComputedTextLength();
        if (textWidth > (width * (100 - markValue)) / 100 - captionDistance) {
          return "end";
        }
        return "start";
      })
      .attr("x", function(): number {
        const textWidth = this.getComputedTextLength();
        if (textWidth > (width * (100 - markValue)) / 100 - captionDistance) {
          return (width * markValue) / 100 - captionDistance;
        }
        return (width * markValue) / 100 + captionDistance;
      });
  });
  return updateFunctions;
};

const Graph: FunctionComponent<GraphTypes> = (props): ReactElement => {
  const { width, ballotsStats, voteStats } = props;
  const graph = useRef<SVGSVGElement>(null);
  const [hasBeenDrawed, setDrawed] = useState(false);
  const [updateGraph, setUpdateGraph] = useState<
    ((width: number, newBarInfo: BarInfo[]) => void)[]
  >();

  useEffect((): void => {
    if (width === 0 || !graph.current || hasBeenDrawed) return;
    setDrawed(true);
    const updateFunctions = drawGraph(graph.current, props);
    setUpdateGraph(updateFunctions);
  }, [width]);

  useEffect((): void => {
    if (!updateGraph) return;
    const barArray = calculateBarsArray(width, ballotsStats, voteStats);
    updateGraph.forEach((item: UpdateGraphCallback): void => {
      item(width, barArray);
    });
  }, [width]);
  return <svg ref={graph} />;
};

interface MajorityGraphTypes {
  className?: string;
  ballotsStats: BallotsStats;
  voteStats: VoteStats;
}

const MajorityGraph: FunctionComponent<MajorityGraphTypes> = ({
  className,
  ballotsStats,
  voteStats,
}): ReactElement => {
  const majorityBallots: BallotsStats = {
    ...ballotsStats,
    pass: 0,
  };
  const majorityVotes: VoteStats = {
    votesAvailable: majorityBallots.yay + majorityBallots.nay,
    votesCast: majorityBallots.yay + majorityBallots.nay,
    numVoters: voteStats.numVoters,
  };

  const { t } = useTranslation();

  return (
    <div className={cx(className, styles.majorityGraph)}>
      <ReactResizeDetector
        handleWidth
        render={({ width = 0 }): ReactElement => (
          <>
            <Graph
              width={width}
              ballotsStats={ballotsStats}
              voteStats={voteStats}
              type="quorum"
              markCaption={t("proposals.majorityGraph.quorumCaption", {
                value: ballotsStats.quorum,
              })}
              markValue={ballotsStats.quorum}
            />
            <Graph
              width={width}
              ballotsStats={majorityBallots}
              voteStats={majorityVotes}
              type="majority"
              markCaption={t("proposals.majorityGraph.supermajorityCaption", {
                value: ballotsStats.supermajority,
              })}
              markValue={ballotsStats.quorum}
            />
          </>
        )}
      />
    </div>
  );
};

export default MajorityGraph;
