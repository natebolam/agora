import React, { FunctionComponent, ReactElement } from "react";
import { useMediaQuery } from "react-responsive";
import Color from "color";

export interface QuorumGraphProps {
  maxValue: number;
  value: number;
  quorumValue: number;
  quorumMarkLabel: string;
}

export const QuorumGraph: FunctionComponent<QuorumGraphProps> = ({
  maxValue,
  value,
  quorumValue,
  quorumMarkLabel,
}): ReactElement => {
  const backgroundColor: Color = Color("rgba(44, 125, 247, 0.1)");
  const voteColorYes: Color = Color("#2eca9b");
  const voteColorNo: Color = Color("#fd2d49");
  const markColor: Color = Color("rgb(18, 50, 98, 0.1)");
  const markLabelColor: Color = Color("rgb(18, 50, 98, 0.5)");

  const isMobile = useMediaQuery({ maxWidth: 850 });
  const barHeight: number = isMobile ? 16 : 24;
  const markHeight = 20;
  const voteWidth = `${(value / maxValue) * 100}%`;

  const mkBar = (width: string, fill: Color): ReactElement => (
    <rect
      x="0"
      y="32"
      width={width}
      height={barHeight}
      rx={barHeight / 2}
      fill={fill.string()}
    />
  );

  const markX = `${(quorumValue / maxValue) * 100}%`;
  const markProps = {
    stroke: markColor.string(),
    "stroke-width": 2,
    x1: markX,
    x2: markX,
    y1: 0,
    y2: markHeight,
  };

  const markLabelOnTheLeft = quorumValue >= maxValue / 2;
  const markLabelProps = {
    x: markX,
    "dominant-baseline": "baseline",
    height: markHeight,
    y: markHeight,
    fill: markLabelColor.string(),
    "text-anchor": markLabelOnTheLeft ? "end" : "start",
    "font-size": 14,
  };
  // A stupid way to add padding with &nbsp;
  const markLabelText =
    (markLabelOnTheLeft ? "" : "\u00A0") +
    quorumMarkLabel +
    (markLabelOnTheLeft ? "\u00A0" : "");

  return (
    <svg width="100%">
      {mkBar("100%", backgroundColor)}
      {mkBar(voteWidth, value >= quorumValue ? voteColorYes : voteColorNo)}

      <line {...markProps} />
      <text {...markLabelProps}>{markLabelText}</text>
    </svg>
  );
};
export default QuorumGraph;
