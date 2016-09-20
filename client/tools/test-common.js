/* This file should be included by any integration specs that need to make http calls */
import 'isomorphic-fetch';

import FormData from 'form-data';
global.FormData = FormData;
import Blob from 'w3c-blob';
global.Blob = Blob;


import {FetchClient, initializeAuthService, retrieveCachedAuthToken, store} from 'cl-ui-common';

class MockLocalStorage {
  _data = {};

  getItem(key: string):string {
    assertString(key, 'key to MockLocalStorage.getItem(key) must be a string.');
    return this._data[key];
  }
  setItem(key: string, value: string):string {
    assertString(key, 'key to MockLocalStorage.setItem(key, value) must be a string.');
    assertString(value, 'value to MockLocalStorage.setItem(key, value) must be a string.');
    return this._data[key] = value;
  }
  removeItem(key: string):string {
    assertString(key, 'key to MockLocalStorage.removeItem(key) must be a string.');
    return delete this._data[key];
  }
}

function assertString(value: string, error: string = 'Must be a string.') {
  if (typeof value !== 'string') {
    throw error + ' Found: ' + typeof value;
  }
}


let mockSessionStore = store(new MockLocalStorage());

FetchClient.instance.configure(config => {
    let tmpConfig = config.set('baseURL', 'http://localhost:9080');
    initializeAuthService({store: mockSessionStore});
    let tmpResolvers = tmpConfig.get('resolvers').push(options => {
        let authToken = retrieveCachedAuthToken();
        return authToken
          ? options.setIn(['headers', 'Authorization'], `Token ${authToken}`)
          : options;
      }
    );

    return tmpConfig.set('resolvers', tmpResolvers);
  }
);


