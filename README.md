# Riak Process Pool


## Quick Start

1.  Configure pool changing settings in priv/pooly.conf
2.  Start up erl

> application:start(pooly).
> {ok, Pid} = pooly:check_out().
> riakc_pb_socket:ping(Pid).
pong
> pooly:check_in(Pid).

## API

{ok, Pid} = pooly:check_out - Checks out a process

pooly:check_in(Pid) - Checks in a process

pooly:size() - Returns the size of the idle queue

pooly:total() - Returns the total number of processes idle and active

## License & Copyright

Copyright &copy;2011 Andrew Berman

Licensed under the Apache License, Version 2.0 (the "License"); 
you may not use this file except in compliance with the License. 
You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under 
the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
either express or implied. See the License for the specific language governing permissions and 
limitations under the License.