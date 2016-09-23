jc_console
====

##Web based monitoring tool for j_cache
jc_console provides a browser monitoring tool allowing a user to understand the basic
health of j_cache as well as see certain details about the tables and cache-lines (bucekts)
that make up a running j_cache cluster.

Jc_console is a dependecny of j_cache and is distributed with j_cache. Having said that,
jc_console will not be started unless a particular j_cache node is configured to start
jc_console(see the configuration section below).


## Front-End / Back-End
The front-end is a React / Redux appliction and is described in its own README in the client
subdirectory. The back-end is an Erlang, OTP-compliant application and is further described in this README.


## Server file structure
jc_console is a standard OTP application and follows the OTP directory convention. Except that there is an additional client directory which contains the front-end React application

```
.
|── client           # root of the browser-based, front-end application 
├── deps             # jc_console application dependencies
├── ebin             # ompiled, jc_console code
├── rel              # Release subdirectory
    |── files          # Template files that get instantiated and moved to the release
    |── jc_console     # jc_console release
├── src              # jc_console source code
```


### Jc_console Technologies
| **Tech** | **Description** | **Sponsor** |
|----------|-------|
| [Erlang](http://www.erlang.org) | A programming language used to build massively scalable soft real-time systems with requirements on high availability. | Ericcson  / Open Source
| [Cowboy](http://ninenines.eu/)  |   Small, fast, modular HTTP server. | Ninenines
| [Lasse](https://github.com/inaka/lasse)|SSE handler for Cowboy. | Inaka
| [Lagger](http://basho.com) | A logging framework for Erlang/OTP | Basho



###Build instructions
* Ensure that Erlang 17 or higher is installed
* Get the Source Code from Stash or bit bucket

      [root@db01] git clone ....
      [root@db01] cd jc_console

* Get dependancies and compile
  
      [root@db01] ./rebar get-deps clean compile
    
* Modify logging if desired in sys.config file
   	  
* Generating a release should not be necessary since it jc_console will be build by j_cache as it is a dependecy of j_cache. But, if you do need to build the release - it will be located in jc_console/rel/jc_console

        [root@db01] cd rel
		[root@db01] ../rebar generate



###Features
Provides JSON endpoints indicating

 * Summary information regarding the health of j_cache
 * Detailed informatin regardng a specific cache_line
 * SSE indicating evicts and writes for a given cache line




###Configuration
* For j_cache nodes that want to offer jc_console, the j_cache sys.config file 
needs two additional lines in the jc stanza

  	   ... snip ...
    	{jc,
          [
           ...snip ...
           %% To enable the web-based console, jc_console set to true.
           {console, false},
           {port, 8080},
           ...snip...
           ]
       }