import { combineReducers } from 'redux';
import {summaries} from './summary/reducers';

const rootReducer = combineReducers({
  summaries
});

export default rootReducer;
