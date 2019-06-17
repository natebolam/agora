import { Actions, TestActions } from "~/store/actions/test-action";

export interface TestStoreType {
  value: number;
}

const initialState: TestStoreType = {
  value: 0
};

export function testReducer(
  state: TestStoreType = initialState,
  action: TestActions
): TestStoreType {
  switch (action.type) {
    case Actions.INCREMENT:
      return {
        value: state.value + 1
      };
    case Actions.DECREMENT:
      return {
        value: state.value - 1
      };
    default:
      return state;
  }
}
