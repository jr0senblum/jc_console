import React, {PropTypes} from 'react';

export const CacheLine = ({cache, rel, sse, onRelClicked}) => (
  <div>
    Name: {cache} <button onClick={(e) => onRelClicked({rel, sse})}>Inspect</button>
  </div>
);

CacheLine.propTypes = {
  cache: PropTypes.string.isRequired,
  rel: PropTypes.string.isRequired,
  sse: PropTypes.string.isRequired,
  onRelClicked: PropTypes.func.isRequired
};



