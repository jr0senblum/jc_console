import { createStore, applyMiddleware, compose } from 'redux';
import thunk from 'redux-thunk';
import {routerMiddleware} from 'react-router-redux';
import {browserHistory} from 'react-router';
import rootReducer from '../reducers';

export default function configureStore() {
  return createStore(rootReducer, compose(applyMiddleware(thunk), applyMiddleware(routerMiddleware(browserHistory))));
}
