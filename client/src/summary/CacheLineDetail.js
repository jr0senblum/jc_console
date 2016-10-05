import React, {PropTypes} from 'react';

export const CacheLineDetail = ({name = "", indexes = [], records = 0, sequence_no = null, ttl = null}) => (
  <div>
    <div>Records: {records}</div>
    <div>Sequence Number: {!sequence_no ? 0 : sequence_no}</div>
    <div>TTL: {!ttl ? 0 : ttl}</div>
    <div>
        {indexes.map(index => <div key={index.path.join()}>Path: {index.path.join('.')}</div>)}
    </div>
  </div>
);

CacheLineDetail.propTypes = {
  name: PropTypes.string.isRequired,
  indexes: PropTypes.array.isRequired,
  records: PropTypes.number.isRequired,
  sequence_no: PropTypes.number,
  ttl: PropTypes.number //not sure what this is
};

