import React, {PropTypes} from 'react';

export const ActivityIndicator = ({activity}) => (
  <div>
    <span>Activity:</span>
    <span>{activity ? '1' : '0'}</span>
  </div>
);

ActivityIndicator.propTypes = {
  activity: PropTypes.bool.isRequired
};
