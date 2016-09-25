/* @flow */
import React, {PropTypes} from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';
import {summary as summary, loadCacheLine, clearCache, addRandom} from './actions';
import {SSE} from './event-consumer';

import {CacheLine, CacheLineDetail, ActivityIndicator} from './';

const mapStateToProps = (state) => (
{
  summaries: state.summaries,
  cacheLine: state.cacheLine
}
);

const mapDispatchToProps = (dispatch) => (
{
  actions: bindActionCreators({
      summaries: summary,
      loadCacheLine,
      clearCache,
      addRandom
    },
    dispatch)
}
);

@connect(mapStateToProps, mapDispatchToProps)
export class Summary extends React.Component {

  sse: SSE;

  constructor(props) {
    super(props);
    this.handleCacheLineSelected = this.handleCacheLineSelected.bind(this);
    this.handleClearCache = this.handleClearCache.bind(this);
    this.handleAddRandom = this.handleAddRandom.bind(this);
    this.sse = new SSE();
    this.state = {sse: undefined}
  }

  componentDidMount() {
    this.props.actions.summaries();
  }

  componentWillUnmount() {
    this.sse.stopAll();
  }

  handleCacheLineSelected(cache) {
    this.props.actions.loadCacheLine("", cache.rel);
    this.sse.stopAll();
    this.sse.start(cache.sse, (data) => this.setState({sse: data}));
  }

  handleClearCache(e) {
    this.props.actions.clearCache();
  }

  handleAddRandom(e) {
    this.props.actions.addRandom();
  }


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
    const {summary} = this.props.summaries;
    const {cacheLine} = this.props.cacheLine;
    console.log("CacheLine: ", cacheLine);
    return (
      <div>

        {
          summary.cache_lines.map(cl =>
            <div key={cl.cache}>
              <CacheLine cache={cl.cache} rel={cl.ref} sse={cl.sse} onRelClicked={this.handleCacheLineSelected}/>
            </div>)
        }

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
          <button onClick={this.handleClearCache}>Clear Cache</button>
          <button onClick={this.handleAddRandom}>Add Random</button>
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
