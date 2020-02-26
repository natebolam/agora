import React, { FunctionComponent, ReactElement } from "react";

const CloseIcon: FunctionComponent = (): ReactElement => (
  <svg width={27} height={26}>
    <path
      d="M2 1l24 24M2 25L26 1"
      fill="none"
      fillRule="evenodd"
      stroke="#fff"
      strokeLinejoin="round"
      strokeWidth={3}
    />
  </svg>
);

export default CloseIcon;
