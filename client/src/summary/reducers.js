import {SUMMARY_LOADED, CACHE_LINE_LOADED, REQUEST_CLEAR_CACHE} from './action-types';

const initialState = {
  summary: {
    cache_lines: [],
    nodes: {
      configured: [],
      up: []
    },
    per_node_sizes: {}
  }
};

export const summaries = (state = initialState, action) => {
  switch (action.type) {
    case SUMMARY_LOADED:
      return {...state, summary: action.payload};
  }
  return state;
};

const initialCacheState = {
  cacheLine: undefined
};

export const cacheLine = (state = initialCacheState, action) => {
  switch (action.type) {
    case CACHE_LINE_LOADED:
      return {
        ...state,
        cacheLine: {...action.payload}};
  }
  return state;
};


const initialViewState = {
  clearCache: false
};

export const view = (state = initialViewState, action) => {

  switch (action.type) {
    case REQUEST_CLEAR_CACHE:
      return {
        ...state,
        clearCache: action.payload
      };
    default:
      return state;
  }

};
