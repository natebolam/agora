import React, { ElementType, FunctionComponent, ReactElement } from "react";

import { Provider } from "react-redux";
import {
  applyMiddleware,
  combineReducers,
  compose,
  createStore,
  Store,
} from "redux";
import ReduxThunk from "redux-thunk";
import { periodReducer, PeriodState } from "~/store/reducers/periodReducer";
import proposalReducer, {
  ProposalState,
} from "~/store/reducers/proposalReducer";

export interface RootStoreType {
  periodStore: PeriodState;
  proposalStore: ProposalState;
}

const rootReducer = combineReducers({
  periodStore: periodReducer,
  proposalStore: proposalReducer,
});

function configureStore(): Store<RootStoreType> {
  const composeEnhancers =
    window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;
  const middlewares = [ReduxThunk];

  const enhancers = composeEnhancers(applyMiddleware(...middlewares));

  return createStore(rootReducer, undefined, enhancers);
}

export function withRedux(Children: React.ElementType): FunctionComponent {
  return function(
    props: React.ComponentPropsWithoutRef<ElementType>
  ): ReactElement {
    return (
      <Provider store={configureStore()}>
        <Children {...props} />
      </Provider>
    );
  };
}
