import React, { FunctionComponent, ReactElement } from "react";
import jdenticon from "jdenticon";

interface NoUserIconTypes {
  className?: string;
  size?: number;
  value?: string;
}

const NoUserIcon: FunctionComponent<NoUserIconTypes> = ({
  className,
  value,
  size = 32,
}): ReactElement => {
  return (
    <div
      className={className}
      dangerouslySetInnerHTML={{
        __html: jdenticon.toSvg(value || Math.random(), size),
      }}
    />
  );
};

export default NoUserIcon;
