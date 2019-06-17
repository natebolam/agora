const INCREMENT = "@@TEST/INCREMENT";
const DECREMENT = "@@TEST/DECREMENT";

export const Actions = {
  INCREMENT,
  DECREMENT
};

export interface IncrementAction {
  type: typeof INCREMENT;
}

export interface DecrementAction {
  type: typeof DECREMENT;
}

export type TestActions = IncrementAction | DecrementAction;

function increment(): IncrementAction {
  return {
    type: INCREMENT
  };
}

function decrement(): DecrementAction {
  return {
    type: DECREMENT
  };
}

export const ActionCreators = {
  increment,
  decrement
};
