import { combineReducers } from 'redux';
import {summaries, cacheLine} from './summary/reducers';

const rootReducer = combineReducers({
  summaries,
  cacheLine
});

export default rootReducer;
