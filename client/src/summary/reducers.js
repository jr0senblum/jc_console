import {SUMMARY_LOADED, LOAD_SUMMARY} from './actionTypes';

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
