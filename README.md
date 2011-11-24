# OTP Process Pool

### Recent changes

#### 11/23/2011
* Simplified architecture and now allow pooly_sup to be passed the config file to allow other OTP applications to supervise pooly.  To do this, supervise pooly_sup and pass it the file name for the pooly configuration.
* Changed application to look for an environment variable to tell it where the pooly.conf file is located.  This should improve portability when included in other applications.

#### 8/14/2011
* Added support for multiple pools
* Added support for default configuration parameters but allow a pool to override a parameter
* Added support for generic OTP compliant pools

## Quick Start

1.  Create a pooly.conf configuration file in the priv directory
2.  Configure one or more pools in the configuration file
3.  Create an app config file for your application which includes:

```
    [{pooly, [{config_file, LocationOfFile}]}].

    e.g. [{pooly, [{config_file, "priv/pooly.conf"}]}].
    This will allow pooly to use your application's priv directory as opposed to its own.
```

4.  Start up erlang using the -config directive passing it the name of your config file.  For example, if your application config file is named, myApp.config, you would type: erl -config myApp

```
> application:start(pooly).
> {ok, Pid} = pooly:check_out(poolName).
> pooly:check_in(poolName, Pid).
```

## Configuration

### Global pool tuning parameters
All parameters are optional.  If the parameter is absent, the default will be used.

+ **acquire_increment** - *Default: 3* - Determines the number of members to acquire at a time when the pool requires more processes to fill the pool
+ **initial_pool_size** - *Default: 5* - Determines the number of members to acquire at startup
+ **max_pool_size** - *Default: infinity* - Determines the maximum number of members the pool will hold.  Any requests above this will yield an error.
+ **min_pool_size** - *Default: 3* - Determines the minimum number of members the pool will hold. The pool will always have this many members in it at any time.
+ **idle_timeout** - *Default: 7200000 (2 hours, Specified in milliseconds or infinity atom)* - Determines the maximum amount of time a member will remain idle. If a member exceeds the time, it does not necessarily determine that it will be killed.  The pool will automatically decide whether it wants to kill the idle process depending on the max and min pool size.
+ **max_age** - *Default: infinity (Specified in milliseconds or infinity atom)* - Determines the maximum amount of time a member will exist.  Sometimes you want the pool to expire stale processes, so you would set this configuration parameter according to how frequently to kill processes that your application considers stale.  If the process is checked out and it is stale, it will be killed upon check in.

### Pool Worker Setup
Any of the above global tuning parameters can be overridden for a specific pool.  The pool will only call start_link on the module specified.

* module - The name of the worker module
* args - Any args that the worker module should be passed

See pooly.conf.sample for a sample configuration file.

## API
Until I document the code, please refer here on how to use Pooly.

#### Check out a process from the pool:
```
pooly:check_out(PoolName)
-> {ok, Pid}
-> {error, pool_exhausted} - The pool has reached maximum capacity as determined by the max_pool_size
```

#### Check in a process from the pool:
```
pooly:check_in(PoolName, Pid) -> ok
```

#### Retrieve the number of available processes:
```
pooly:idle_count(PoolName) -> {ok, integer()}
```

#### Retrieve the number of busy processes:
```
pooly:busy_count(PoolName) -> {ok, integer()}
```

#### Returns the total number of workers the pool is managing (busy + available):
```
pooly:total_count(PoolName) -> {ok, integer()}
```

## License & Copyright

Copyright &copy;2011 Andrew Berman

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under
the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
either express or implied. See the License for the specific language governing permissions and
limitations under the License.
