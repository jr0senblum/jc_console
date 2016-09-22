jc_console
====

##Web based monitoring tool for j_cache
jc_console is a dependecny of j_cache and is automatically down-loaded, compiled,
etc. when j_cache is installed. While it is included in the distribution, it
will not be started unless the j_cache configuration file indicates that it should
be (see configuration section, below).


###Build Instructions
* Ensure that Erlang 17 or higher is installed
* Get the Source Code from Stash or bit bucket

      [root@db01] git clone ....
      [root@db01] cd jc_console

* Get dependancies and compile
  
      [root@db01] ./rebar get-deps clean compile
    
* Modify logging if desired in sys.config file
   	  
* Generate the release from the rel directory. The release will then be 
  located in jc_console/rel/jc_console

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
