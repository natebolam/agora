import React from "react";

import { Provider } from "react-redux";
import { applyMiddleware, combineReducers, compose, createStore, Store } from "redux";
import ReduxThunk from "redux-thunk";
import { testReducer, TestStoreType } from "~/store/reducers/test-reducer";


export interface RootStoreType {
  test: TestStoreType
}

const rootReducer = combineReducers({
  test: testReducer
});

function configureStore(): Store<RootStoreType> {
  const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;
  const middlewares = [ReduxThunk];

  const enhancers = composeEnhancers(applyMiddleware(...middlewares));

  return createStore(rootReducer, undefined, enhancers);
}

export function withRedux(Children: React.ElementType) {
  return function (props: React.ComponentPropsWithoutRef<any>): JSX.Element {
    return (
      <Provider store={configureStore()}>
        <Children {...props}/>
      </Provider>
    )
  }
}