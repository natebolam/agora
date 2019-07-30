import React, { ReactElement } from "react";

const SvgArrow = (): ReactElement => (
  <svg width={18} height={18}>
    <g fill="none" fillRule="evenodd" stroke="#FFF" strokeWidth={2}>
      <path strokeLinejoin="round" d="M2 9h16" />
      <path d="M10 17L2 9l8-8" />
    </g>
  </svg>
);

export default SvgArrow;
