import React, {
  FunctionComponent,
  ReactElement,
  useEffect,
  useRef,
} from "react";
import Card from "~/components/common/Card";
import styles from "~/styles/components/proposals/ProposalPieChart.scss";
import DiscourseButton from "~/components/controls/DiscourseButton";
import * as d3 from "d3";
import { ProposalsListItem } from "~/models/ProposalsList";
import ReactResizeDetector from "react-resize-detector";
import { useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import { useTranslation } from "react-i18next";
import { ProposalPeriodInfo } from "~/models/Period";

const colors: string[] = [
  "#2ed47a",
  "#c43290",
  "#f6a41e",
  "#3676f8",
  "#e4572e",
];

interface PieChartItem {
  title: string;
  value: number;
  color: string;
}

class PieChartItemImpl implements PieChartItem {
  public title: string = "";
  public value: number = 0;
  public color: string = "";

  public valueOf(): number {
    return this.value;
  }
}

const processPieChartData = (
  proposals: ProposalsListItem[] | null
): PieChartItem[] => {
  if (!proposals) {
    return [];
  }
  return proposals
    .map(
      (proposal: ProposalsListItem): PieChartItem => ({
        title: proposal.title ? proposal.title : proposal.hash,
        value: proposal.votesCasted,
        color: "",
      })
    )
    .sort((a, b): number => b.value - a.value)
    .filter((item): boolean => item.value !== 0);
};

const joinRestItems = (pieChartItems: PieChartItem[]): PieChartItem[] => {
  const { t } = useTranslation();

  if (pieChartItems.length <= colors.length) {
    return pieChartItems;
  }
  const restVotesCasted = pieChartItems
    .slice(colors.length - 1)
    .map((item): number => item.value)
    .reduce(
      (previousValue, currentValue): number => previousValue + currentValue,
      0
    );
  return [
    ...pieChartItems.slice(0, colors.length - 1),
    {
      title: t("proposals.pieChart.othersCaption"),
      value: restVotesCasted,
      color: "",
    },
  ].filter((item): boolean => item.value !== 0);
};

const assignColor = (pieChartItems: PieChartItem[]): PieChartItem[] => {
  return pieChartItems.map(
    (value, index): PieChartItem => {
      return {
        ...value,
        color: colors[index],
      };
    }
  );
};

const drawGraph = (
  svg: SVGSVGElement,
  width: number,
  data: PieChartItem[]
): void => {
  const pieChartSize = Math.min(224, width);
  const pieRadius = pieChartSize / 2;

  const pieChart = d3
    .select(svg)
    .attr("width", `100%`)
    .attr("height", `${pieChartSize}px`);

  pieChart.select("g").remove();

  const wrapper = pieChart
    .append("g")
    .attr("width", "100%")
    .attr("height", "100%")
    .style("transform", `translate(${pieRadius}px, 50%)`);

  const arc = d3.pie().value((d): number => (d as PieChartItemImpl).value)(
    data as PieChartItemImpl[]
  );

  const totalVotes = data
    .map((item: PieChartItem): number => item.value)
    .reduce((previousValue, currentValue): number => {
      return previousValue + currentValue;
    }, 0);

  wrapper
    .selectAll("arc")
    .data(arc)
    .enter()
    .append("path")
    .attr("d", d3
      .arc()
      .innerRadius(pieRadius - 8)
      .outerRadius(pieRadius)
      .cornerRadius(10)
      .startAngle((a): number => {
        return a.startAngle - 0.1;
        // Intentionally made cast to any for a sake of D3
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      }) as any)
    .attr("fill", (datum): string => {
      return (datum.data as PieChartItemImpl).color;
    })
    .style("transform", "rotate(180deg)");
  if (data.length > 0) {
    wrapper
      .append("text")
      .html(`${((data[0].value / totalVotes) * 100).toFixed(1)}%`)
      .attr("text-anchor", "middle")
      .attr("font-size", `${56 * Math.min(width / 224, 1)}px`)
      .attr("dominant-baseline", "middle")
      .style("font-weight", 500)
      .attr("fill", colors[0]);
  }
};

interface PieChartGraphCanvasTypes {
  width: number;
  pieChartData: PieChartItem[];
}

const PieChartGraphCanvas: FunctionComponent<PieChartGraphCanvasTypes> = ({
  width,
  pieChartData,
}): ReactElement => {
  const graph = useRef<SVGSVGElement>(null);

  useEffect((): void => {
    if (graph.current && width !== 0) {
      drawGraph(graph.current, width, pieChartData);
    }
  }, [width]);
  return <svg ref={graph} />;
};

const PieChartGraph: FunctionComponent<{
  pieChartData: PieChartItem[];
}> = ({ pieChartData }): ReactElement => {
  return (
    <div className={styles.pieChart__graph}>
      <ReactResizeDetector
        handleWidth
        render={({ width = 0 }): ReactElement => (
          <PieChartGraphCanvas pieChartData={pieChartData} width={width} />
        )}
      />
    </div>
  );
};

const PieChartLegend: FunctionComponent<{ pieChartData: PieChartItem[] }> = ({
  pieChartData,
}): ReactElement => {
  return (
    <div className={styles.pieChart__legend}>
      <ul>
        {pieChartData.map(
          (item, index): ReactElement => (
            <li key={index}>
              <div
                className={styles.pieChart__legend__circle}
                style={{ borderColor: item.color }}
              />
              {item.title}
            </li>
          )
        )}
      </ul>
    </div>
  );
};

const PieChartBody: FunctionComponent = (): ReactElement => {
  const proposals = useSelector((state: RootStoreType):
    | ProposalsListItem[]
    | null => {
    return state.periodStore.proposals;
  });

  const pieChartData = assignColor(
    joinRestItems(processPieChartData(proposals))
  );

  const discourseLink = useSelector((state: RootStoreType): string => {
    return state.periodStore.period &&
      (state.periodStore.period as ProposalPeriodInfo).discourseLink
      ? (state.periodStore.period as ProposalPeriodInfo).discourseLink
      : "#";
  });

  return (
    <div className={styles.pieChart__body}>
      <div className={styles.pieChart__body__main}>
        <PieChartGraph pieChartData={pieChartData} />
        <PieChartLegend pieChartData={pieChartData} />
      </div>
      <div className={styles.pieChart__discourse}>
        <DiscourseButton href={discourseLink} />
      </div>
    </div>
  );
};

interface ProposalPieChartTypes {
  className?: string;
}

const ProposalPieChart: FunctionComponent<ProposalPieChartTypes> = ({
  className,
}): ReactElement => {
  return (
    <Card className={className} header={"Proposals"} body={<PieChartBody />} />
  );
};

export default ProposalPieChart;
