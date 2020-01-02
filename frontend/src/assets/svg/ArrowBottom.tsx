import React, { FunctionComponent, ReactElement } from "react";

interface ArrowBottomProps {
  className?: string;
}

const ArrowBottomSvg: FunctionComponent<ArrowBottomProps> = ({
  className,
}): ReactElement => (
  <svg width={12} height={6} className={className}>
    <path fill="#fff" fillRule="evenodd" d="M0 0h12L6 6z" opacity={0.6} />
  </svg>
);

export default ArrowBottomSvg;
