import React from 'react';
import {render} from 'react-dom';

import {BrowserRouter, Match, Miss, Link} from 'react-router';
import {AppBar, Menu, MenuItem} from 'material-ui';

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';

import './styles/main.scss';

const muiTheme = getMuiTheme({
  appBar: {
    height: 56,
    color: '#ccc'
  }
});
// 1. import a few components

const App = () => (
  <MuiThemeProvider muiTheme={muiTheme}>

    <BrowserRouter>

        <div className="row">

          <div className="col-xs-12">
            <AppBar title="JCachet"
                    iconClassNameRight="muidocs-icon-navigation-expand-more" />
          </div>

          <div className="col-sm-3">

            <ul>
              <li><Link to="/">Home</Link></li>
              <li><Link to="/about">About</Link></li>
              <li><Link to="/topics">Topics</Link></li>
            </ul>
          </div>

          <div className="col-sm-9">
            <Match exactly pattern="/" component={Home}/>
            <Match pattern="/repo" component={About}/>
            <Match pattern="/topics" component={Topics}/>
            <Miss component={NoMatch}/>
          </div>
        </div>
    </BrowserRouter>
  </MuiThemeProvider>
);

const Home = () => (
  <div>
    <h2>Home</h2>
  </div>
);

const About = () => (
  <div>
    <h2>About</h2>
  </div>
);

const NoMatch = ({location}) => (
  <div>
    <h2>Whoops</h2>
    <p>Sorry but {location.pathname} didn’t match any pages</p>
  </div>
);

const Topics = ({pathname, pattern}) => (
  // 5. Components rendered by a `Match` get some routing-specific
  //    props, like the portion of the parent `pattern` that was
  //    matched against the current `location.pathname`, in this case
  //    `/topics`
  <div>
    <h2>Topics</h2>
    <ul>
      {/* 6. Use the parent's matched pathname to link relatively */}
      <li><Link to={`${pathname}/rendering`}>Rendering with React</Link></li>
      <li><Link to={`${pathname}/components`}>Components</Link></li>
      <li><Link to={`${pathname}/props-v-state`}>Props v. State</Link></li>
    </ul>

    {/* 7. Render more `Match` components to get nesting naturally
     within the render lifecycle. Use the parent's matched
     pathname to nest the url.
     */}
    <Match pattern={`${pathname}/:topicId`} component={Topic}/>

    {/* 8. use the `render` prop for convenient inline rendering */}
    <Match pattern={pathname} exactly render={() => (
      <h3>Please select a topic</h3>
    )}/>
  </div>
);

const Topic = ({params}) => (
  // 9. the dynamic segments of a `pattern` (in this case `:topicId`)
  //    are parsed and sent to the component from `Match`.
  <div>
    <h3>{params.topicId}</h3>
  </div>
);

render(<App/>, document.querySelector('#j-cachet-app'));
