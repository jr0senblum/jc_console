import { combineReducers } from 'redux';
import {summaries, cacheLine, view} from './summary/reducers';

const rootReducer = combineReducers({
  summaries,
  cacheLine,
  view
});

export default rootReducer;
