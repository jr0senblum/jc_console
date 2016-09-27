/* @flow */
import React, {PropTypes} from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';
import {mockSummary as summary} from './actions';


import {CacheLine} from './';


const mapStateToProps = (state) => (
  {
    summaries: state.summaries
  }
);


const mapDispatchToProps = (dispatch) => (
  {
    actions: bindActionCreators({
        summaries: summary
      },
    dispatch)
  }
);

@connect(mapStateToProps, mapDispatchToProps)
export class Summary extends React.Component {



  constructor(props) {
    super(props);
  }

  componentDidMount() {
    this.props.actions.summaries();
  }

  render() {
    const {summary} = this.props.summaries;
    return (
      <div>
        <ul>
          {summary.cache_lines.map(cl => <CacheLine key={cl.cache} cache={cl.cache} rel={cl.ref} sse={cl.sse} />)}
        </ul>
      </div>
    );
  }
}

Summary.propTypes = {
  actions: PropTypes.shape({
    summaries: PropTypes.func
  }),
  summaries: PropTypes.shape({
    summary: PropTypes.object //yes we can get more specific, this will come later.
  })
};
