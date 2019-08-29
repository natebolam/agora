import React, { FunctionComponent, ReactElement } from "react";
import jdenticon from "jdenticon";

interface NoUserIconTypes {
  className?: string;
  size?: number;
}

const NoUserIcon: FunctionComponent<NoUserIconTypes> = ({
  className,
  size = 32,
}): ReactElement => {
  return (
    <div
      className={className}
      dangerouslySetInnerHTML={{ __html: jdenticon.toSvg(Math.random(), size) }}
    />
  );
};

export default NoUserIcon;
