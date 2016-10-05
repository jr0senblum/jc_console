import React, {PropTypes} from 'react';

export const NodeTable = ({tables}) => (
  <table>
    <thead>
    <tr>
      <td>Table Name</td>
      <td>Record Count</td>
      <td>Byte Count</td>
    </tr>
    </thead>
    <tbody>
    {tables.map(t => <NodeRow {...t} key={t.table_name} />)}
    </tbody>
  </table>
);

const NodeRow = ({byte_count, record_count, table_name}) => (
  <tr>
    <td>{table_name}</td>
    <td>{record_count}</td>
    <td>{byte_count}</td>
  </tr>
);

NodeTable.propTypes = {
  tables: PropTypes.array
};

NodeRow.propTypes = {
  byte_count: PropTypes.number.isRequired,
  record_count: PropTypes.number.isRequired,
  table_name: PropTypes.string.isRequired
};
