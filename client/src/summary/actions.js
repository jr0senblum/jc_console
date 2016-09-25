import axios from 'axios';
import {SUMMARY_LOADED, CACHE_LINE_LOADED, CACHE_CLEARED, RANDOM_ADDED} from './action-types';

export function summary() {
  return dispatch =>
    axios.get('/api/summary')
      .then(summary => {
        console.log("Summary: ", summary);
        dispatch({
          type: SUMMARY_LOADED,
          payload: summary.data
        })});
}

export function loadCacheLine(name, cacheLineURL) {
  return dispatch =>
    axios.get(cacheLineURL)
      .then(data =>
        dispatch({
          type: CACHE_LINE_LOADED,
          payload: {
            name,
            ...data.data
          }
        }));
}

export function clearCache() {
  return dispatch =>
    axios.post('/api/command', {command: 'clear'},{
      headers: {
        "Content-Type": "application/x-www-form-urlencoded"
      }})
      .then(data => dispatch({type: CACHE_CLEARED}));
}

export function addRandom() {
  return dispatch =>
    axios.post('/api/command', {command: 'random'},{
      headers: {
        "Content-Type": "application/x-www-form-urlencoded"
      }})
      .then(data => dispatch({type: RANDOM_ADDED}));
}



export function mockSummary() {

  return {

    type: SUMMARY_LOADED,
    payload: {
      "cache_lines": [
        {
          "cache": "bed",
          "ref": "http://localhost:3010/api/map/bed",
          "sse": "http://localhost:3010/api/eventsource/map/bed"
        },
        {
          "cache": "evs",
          "ref": "http://localhost:3010/api/map/evs",
          "sse": "http://localhost:3010/api/eventsource/map/evs"
        }
      ],
      "nodes": {
        "configured": [
          "jcache@127.0.0.1"
        ],
        "up": [
          "jcache@127.0.0.1"
        ]
      },
      "per_node_sizes": {
        "jcache@127.0.0.1": {
          "tables": [
            {
              "byte_count": 12512,
              "record_count": 10,
              "table_name": "schema"
            },
            {
              "byte_count": 2448,
              "record_count": 1,
              "table_name": "seq"
            },
            {
              "byte_count": 2552,
              "record_count": 1,
              "table_name": "to_index"
            },
            {
              "byte_count": 2720,
              "record_count": 1,
              "table_name": "auto_index"
            },
            {
              "byte_count": 2384,
              "record_count": 0,
              "table_name": "ttl"
            },
            {
              "byte_count": 2384,
              "record_count": 0,
              "table_name": "max_ttl"
            },
            {
              "byte_count": 2600,
              "record_count": 2,
              "table_name": "stats"
            },
            {
              "byte_count": 2384,
              "record_count": 0,
              "table_name": "ps_sub"
            },
            {
              "byte_count": 2384,
              "record_count": 0,
              "table_name": "ps_client"
            },
            {
              "byte_count": 40704,
              "record_count": 200,
              "table_name": "key_to_value"
            }
          ]
        }
      },
      "up_time": {
        "now": "Fri, 23 Sep 2016 17:43:58 GMT",
        "started": "Thu, 22 Sep 2016 17:05:15 GMT",
        "up": {
          "days": 1,
          "hours": 0,
          "minutes": 38,
          "seconds": 43
        }
      }
    }
  };
}
