/* This file should be included by any integration specs that need to make http calls */
import 'isomorphic-fetch';

import FormData from 'form-data';
global.FormData = FormData;
import Blob from 'w3c-blob';
global.Blob = Blob;

function assertString(value: string, error: string = 'Must be a string.') {
  if (typeof value !== 'string') {
    throw error + ' Found: ' + typeof value;
  }
}
