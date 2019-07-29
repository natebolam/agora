import React, { ReactElement } from "react";

const InfoIcon = (): ReactElement => (
  <svg width={12} height={12}>
    <g fill="none" fillRule="evenodd" stroke="#2C7DF7">
      <circle cx={6} cy={6} r={5.5} />
      <path d="M6 3v1m0 1v4" />
    </g>
  </svg>
);

export default InfoIcon;
