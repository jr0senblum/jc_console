import React, {PropTypes} from 'react';
import {render} from 'react-dom';
import configureStore from './store/configureStore';
import {Provider} from 'react-redux';

import {BrowserRouter, Match, Miss, Link} from 'react-router';

import {AppBar, Menu, MenuItem} from 'material-ui';

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';

import {Summary} from './summary/Summary';


import './styles/main.scss';


const store = configureStore();


const muiTheme = getMuiTheme({
  appBar: {
    height: 56,
    color: '#ccc'
  }
});

const App = () => (
  <Provider
    store={store}>
    <MuiThemeProvider muiTheme={muiTheme}>

      <BrowserRouter>

        <div className="row">

          <div className="col-xs-12">
            <AppBar title="JCachet"
                    iconClassNameRight="muidocs-icon-navigation-expand-more"/>
          </div>

          <div className="col-sm-3">

            <ul>
              <li><Link to="/">Home</Link></li>
              <li><Link to="/summary">Summary</Link></li>
            </ul>
          </div>

          <div className="col-sm-9">
            <Match exactly pattern="/" component={Home}/>
            <Match exactly pattern="/summary" component={Summary}/>
            <Miss component={NoMatch}/>
          </div>
        </div>
      </BrowserRouter>
    </MuiThemeProvider>
  </Provider>
);

const Home = () => (
  <div>
    <h2>Home</h2>
  </div>
);


const NoMatch = ({location}) => (
  <div>
    <h2>Whoops</h2>
    <p>Sorry but {location.pathname} didn’t match any pages</p>
  </div>
);

NoMatch.propTypes = {
  location: PropTypes.shape({
    location: PropTypes.string
  })
};


render(<App/>, document.querySelector('#j-cachet-app'));

