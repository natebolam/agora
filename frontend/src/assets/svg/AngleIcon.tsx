import React, { FunctionComponent, ReactElement } from "react";

interface Props {
  className?: string;
}

const AngleIcon: FunctionComponent<Props> = ({ className }): ReactElement => (
  <svg width={11} height={18} className={className}>
    <path
      fill="none"
      stroke="#fff"
      strokeWidth={2}
      d="M1 1l8 8-8 8"
      opacity={0.6}
    />
  </svg>
);

export default AngleIcon;
