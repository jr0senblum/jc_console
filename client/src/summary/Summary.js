/* @flow */

import React, {PropTypes} from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';
import {summary as summary, loadCacheLine, clearCache, addRandom, requestClearCache} from './actions';
import {SSE} from './event-consumer';

import {CacheLine, CacheLineDetail, ActivityIndicator, NodeTable} from './';
import {Dialog, FlatButton, RaisedButton}  from 'material-ui';

const mapStateToProps = (state) => ({
  summaries: state.summaries,
  cacheLine: state.cacheLine,
  clearCache: state.view.clearCache
});

const mapDispatchToProps = (dispatch) => ({
  actions: bindActionCreators({
    summaries: summary,
    loadCacheLine,
    clearCache,
    addRandom,
    requestClearCache
  },
  dispatch)
});

@connect(mapStateToProps, mapDispatchToProps)
export class Summary extends React.Component {

  sse: SSE;

  constructor(props) {
    super(props);
    this.handleCacheLineSelected = this.handleCacheLineSelected.bind(this);
    this.handleClearCache = this.handleClearCache.bind(this);
    this.handleAddRandom = this.handleAddRandom.bind(this);
    this.sse = new SSE();
    this.state = {sse: undefined};
  }

  componentDidMount() {
    this.props.actions.summaries();
  }

  componentWillUnmount() {
    this.sse.stopAll();
  }

  handleCacheLineSelected(e) {
    const url = e.target.value;
    this.props.actions.loadCacheLine(url);
  }

  componentWillReceiveProps(props) {
    console.log("Props changed to: ", props);
  }

  handleClearCache(e) {
    this.props.actions.clearCache()
      .then(() => this.props.actions.summaries())
      .then(() => this.props.actions.requestClearCache(false));
  }

  handleAddRandom(e) {
    this.props.actions.addRandom();
  }

  /**
   {
    "cache_lines": [
        {
            "cache": "trx",
            "ref": "/api/map/trx",
            "sse": "/api/eventsource/map/trx"
        },
        {
            "cache": "bed",
            "ref": "/api/map/bed",
            "sse": "/api/eventsource/map/bed"
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
                    "byte_count": 2384,
                    "record_count": 0,
                    "table_name": "seq"
                },
                {
                    "byte_count": 2552,
                    "record_count": 1,
                    "table_name": "to_index"
                },
                {
                    "byte_count": 2384,
                    "record_count": 0,
                    "table_name": "auto_index"
                },
                {
                    "byte_count": 2384,
                    "record_count": 0,
                    "table_name": "ttl"
                },
                {
                    "byte_count": 2448,
                    "record_count": 1,
                    "table_name": "max_ttl"
                },
                {
                    "byte_count": 2600,
                    "record_count": 2,
                    "table_name": "stats"
                },
                {
                    "byte_count": 2872,
                    "record_count": 1,
                    "table_name": "ps_sub"
                },
                {
                    "byte_count": 2496,
                    "record_count": 1,
                    "table_name": "ps_client"
                },
                {
                    "byte_count": 1504,
                    "record_count": 4,
                    "table_name": "key_to_value"
                }
            ]
        }
    },
    "up_time": {
        "now": "Wed, 05 Oct 2016 21:20:01 GMT",
        "started": "Thu, 29 Sep 2016 21:35:20 GMT",
        "up": {
            "days": 5,
            "hours": 23,
            "minutes": 44,
            "seconds": 41
        }
    }
}



   */


  debounce(func, wait, immediate) {
    let timeout;

    return () => {
      var context = this, args = arguments;
      var later = function () {
        timeout = null;
        if (!immediate) func.apply(context, args);
      };
      const callNow = immediate && !timeout;
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
      if (callNow) func.apply(context, args);
    };
  }

  render() {
    const {
      summary,
      summary: {per_node_sizes},
    } = this.props.summaries;

    const {clearCache} = this.props;
    const {requestClearCache} = this.props.actions;
    const {cacheLine} = this.props.cacheLine;

    console.log("Cache line is: ", cacheLine);

    return (
      <div>

        <select onChange={this.handleCacheLineSelected}>
          {
            summary.cache_lines.map(cl => {
                return (<option key={cl.cache} value={cl.ref}>{cl.cache}</option>)
              }
            )
          };
        </select>

        {
          cacheLine && <CacheLineDetail name={cacheLine.name}
                                        indexes={cacheLine.indexes}
                                        sequence_no={cacheLine.sequence_no}
                                        records={cacheLine.records}
                                        ttl={cacheLine.ttl}/>
        }


        <div>
          {
            cacheLine && <ActivityIndicator activity={false}/>
          }
        </div>

        <div>
          { JSON.stringify(this.state.sse) }
        </div>

        <div>
          <button onClick={() => requestClearCache(true)}>Clear Cache</button>
          <button onClick={this.handleAddRandom}>Add Random</button>
        </div>

        <Dialog
          title="Clear Cache?"
          actions={[
             <FlatButton
              label="Cancel"
              primary={true}
              onClick={() => requestClearCache(false)}
             />,
             <FlatButton
              label="Submit"
              primary={true}
              keyboardFocused={true}
              onClick={this.handleClearCache}
              />
          ]}
          modal={false}
          open={clearCache}
          onRequestClose={() => {}}>
          Are you sure want to clear the cache?
        </Dialog>


        <div>
          {Object.entries(per_node_sizes).map(kv =>
            <div key={kv[0]}>
              <strong>{kv[0]}</strong>
              <NodeTable tables={kv[1]['tables']}/>
            </div>
          )}

        </div>
      </div>
    );
  }
}

Summary.propTypes = {
  actions: PropTypes.shape({
    summaries: PropTypes.func.isRequired,
    loadCacheLine: PropTypes.func.isRequired,
    clearCache: PropTypes.func.isRequired,
    addRandom: PropTypes.func.isRequired
  }),
  summaries: PropTypes.shape({
    summary: PropTypes.object //yes we can get more specific, this will come later.
  })
};
