import React, {PropTypes} from 'react';

export const CacheLine = ({cache, rel, sse}) => (
  <div>
    Name: {cache} (<a href={rel}>ref</a>) | (<a href={sse}>sse</a>)
  </div>
);

CacheLine.propTypes = {
  cache: PropTypes.string.isRequired,
  rel: PropTypes.string.isRequired,
  sse: PropTypes.string.isRequired
};



