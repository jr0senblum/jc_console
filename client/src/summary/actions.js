import axios from 'axios';

function summary() {
  return dispatch =>
    axios.get('api/jcache/summary')
      .then(summary =>
        dispatch({
          type: 'SUMMARY_LOADED',
          value: summary
        })
      );
}
