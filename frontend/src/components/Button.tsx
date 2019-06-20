import React from "react";

interface ButtonProps {
  children: React.ReactNode;
}

export default function Button(props: ButtonProps): React.FunctionComponentElement<ButtonProps> {
  return <button>{props.children}</button>
}