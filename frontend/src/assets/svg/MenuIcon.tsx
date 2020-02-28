import React, { ReactElement } from "react";

const MenuIcon = (): ReactElement => (
  <svg width={24} height={20}>
    <path
      d="M0 2h24M0 10h24M0 18h24"
      fill="none"
      fillRule="evenodd"
      stroke="#fff"
      strokeLinejoin="round"
      strokeWidth={3}
    />
  </svg>
);

export default MenuIcon;
