import * as React from "react";
import "~/styles/main.scss";
import "~/i18n/index";

const Wrapper: React.FunctionComponent<React.HTMLAttributes<HTMLDivElement>> = (
  props
): React.ReactElement => (
  <div {...props} style={{ padding: "2rem", ...props.style }}>
    {props.children}
  </div>
);

export default Wrapper;
