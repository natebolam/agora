import React, { FunctionComponent, ReactElement } from "react";

interface PointerIconProps {
  className?: string;
}

const PointerIconSvg: FunctionComponent<PointerIconProps> = ({
  className,
}): ReactElement => (
  <svg width={8} height={9} className={className}>
    <g
      fill="none"
      fillRule="evenodd"
      stroke="#123262"
      strokeWidth="1.4"
      opacity=".5"
    >
      <path d="M1 5l3 3 3-3M4 8V0" />
    </g>
  </svg>
);

export default PointerIconSvg;
