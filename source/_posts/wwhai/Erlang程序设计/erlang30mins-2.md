---
title: 30分钟学Erlang (一)
date:  2021-01-16 20:02:02
index_img: /static/19.jpg
tags:
- Erlang
- OTP
categories:
- Erlang程序设计
---

作者：Shawn # 概要：使用 erlang:spawn/1,2,3,4 用来创建一个 erlang 进程。Erlang 进程不是操作系统的进程，而是类似其他语言里“协程”的概念，它由 Erlang 虚拟机调度。本文以后说“进程”，就是指 Erlang 进程。
<!-- more -->

> 本文来自我的同事Shawn大佬的总结，Shawn大佬是目前国内Erlang领域内的权威开发架构师，30分钟学erlang系列文章是Shawn的一线经验，对于新入门的朋友很有帮助。再次感谢Shawn花时间整理博客，和贡献文档供社区学习。

# 30分钟学Erlang (二)

## 并发

##### 创建进程
使用 erlang:spawn/1,2,3,4 用来创建一个 erlang 进程。Erlang 进程不是操作系统的进程，而是类似其他语言里“协程”的概念，它由 Erlang 虚拟机调度。本文以后说“进程”，就是指 Erlang 进程。

进程之间是互相独立的，一个进程要想与另外一个进程通信，就必须通过消息传递。消息会被发送到对方进程的信箱存储起来，对方进程可以在合适的时间，按照自定的顺序读取信箱里的消息。

Erlang 里进程非常轻量，启动速度很快，并且可以同时运行千千万万个，默认的进程个数上限是 262144 ，但可以在启动时使用 erl +P 修改这个配置。

``` shell
1> HelloParallel = fun() -> io:format("hello parallel!~n") end.
#Fun<erl_eval.20.99386804>
2> spawn(HelloParallel).  %% spawn/1 BIF 接受一个函数做为参数。
hello parallel!
<0.63.0>
3> PID = pid(0,63,0).   %% 使用 pid 来生成一个 PID
4> is_pid(PID).  %% 检查是否是 PID 类型
true
5> is_process_alive(PID). %%  检查 Process 是否还活着。显示 false 是因为它已经运行完成终止了。
false
```
spawn 函数返回一个新进程的 pid，我们可以使用这个 pid 与其交互。

erlang shell 也是有 pid 的。前面说到一个运行时错误会使得当前的shell 进程崩溃，并重新启动一个新的进程，我们验证一下：
```shell
1> self().   %% self/1 返回当前进程的 pid
<0.60.0>
2> 1 = 2.
** exception error: no match of right hand side value 2
3> self().
<0.63.0>
```

#### 消息发送和接收

使用消息发送运算符 `!` 发送消息。
```shell
4> self() ! "hello".    %% 向自己所在的进程发送一个 List 类型的 "hello". `!` 操作的返回值是消息内容, "hello".
"hello"
5> flush().  %% flush() 将当前 process 的信箱里的所有消息清空并打印。
Shell got "hello"
ok
```

receive ... end 语句使用 pattern matching 来从自己进程的信箱里读取消息，可以使用 after 语句来设置等待超时时间：
```shell
1> self() ! "msg1".
"msg1"
2> self() ! "msg2".
"msg2"
3> self() ! "msg3".
"msg3"
4> receive Msg -> Msg after 3000 -> no_more end. %% 读取任意消息并返回这条消息，如果信箱里没有消息了，等待 3 秒后结束并返回 no_more.
"msg1"
5> receive Msg -> Msg after 3000 -> no_more end.  %% 后面这两条为什么返回 no_more ? 不应该是 "msg2", "msg3" 吗？
no_more
6> receive Msg -> Msg after 3000 -> no_more end.
no_more
```

上面的第 4 行 `receive` 语句里，erlang shell 进程查看邮箱，查到第一个消息是 "msg1", Msg 被绑定为 "msg1"。再次运行 receive 语句的时候，由于 Msg 的值已经为 "msg1"，与信箱里的 "msg2", "msg3" 都不匹配，所以后面两条 `receive` 语句都没有从信箱里读取新消息，"msg2" 和 "msg3" 仍然存储在信箱里:

```shell
16> flush().
Shell got "msg2"
Shell got "msg3"
ok
```
注意虽然后面两个 `receive` 语句都没有从信箱里读取消息，但在 receive 语句的执行过程中，它仍然是从头到尾遍历了整个邮箱，并尝试拿邮箱里的各个消息跟代码里的 `Msg` 进行匹配，这是消耗资源的，等后面消息堆积越多越麻烦。这个叫 `Selective Message Reception`. 消息的读取顺序是接收方决定的。

所以一般情况下我们在读取信箱消息时，读到我们不感兴趣的消息也取出来，打个 error log 然后扔掉它，不要让它一直在信箱里耗费资源。


在 Erlang shell 已经伸展不开拳脚了。让我们来写个复杂点的程序：
我们的程序实现一个 **消息缓存**，具体需求是：
- 我们需要一个消息栈，用于存储用户发来的消息。
- 考虑到用户发来的消息可能有很多，我们需要好几个这样的消息栈来分担负载。
- 我们还想能够给消息栈命名，以便区分。

```erlang
-module(msg_cache).

%% APIs
-export([start_one/1]).

%% for spawns
-export([loop/1]).

%% 定义进程的 state。
%% 我们一般说，一个服务、或 “对象” 会维护自己内部的 '状态'
%% 状态可能是一个字符串缓存，可能是某个资源的引用，这个跟业务相关。
%% 状态存在于内存中，跟外界隔离，通过 API 接口与外界交互。
%% 面向对象语言里用 类和对象来存储状态，Erlang 里我们用 process。
%% 所以我们又说 Erlang 是 “面向Process 编程的”
-record(state, {
            name,      %% 消息栈的名字
            length = 0,  %% 消息栈长度
            buff = []   %% 消息栈的存储列表
         }).

loop(State = #state{name = Name, length = Len, buff = Buff}) ->
  receive
    {get_name, From}->
      From ! {ok, Name},
      loop(State);
    {get_length, From}->
      From ! {ok, Len},
      loop(State);
    {set_name, NewName, From} ->
      From ! ok,
      loop(State#state{name = NewName});
    {push, Msg, From} ->
      From ! ok,
      loop(State#state{buff = [Msg | Buff], length = Len + 1});
    {pop, [], From} ->
      From ! {error, empty},
      loop(State);
    {pop, [TopMsg | Msgs], From} ->
      From ! {ok, TopMsg},
      loop(State#state{buff = Msgs, length = Len - 1});
    _Unsupported ->
      erlang:error(io_libs:format("unsupported msg: ", [_Unsupported]) )
  end.

start_one(BuffName) ->
  %% 启动一个消息栈，并返回其 PID
  Pid = spawn(msg_cache, loop, [#state{name=BuffName}]),
  io:format("Buff ~s created! Pid = ~p~n", [BuffName, Pid]),
  Pid
```

其实除了 loop/1 长一点，其他的都挺容易理解的。
注意 loop/1 里的每个分支的最后一个语句都是尾递归，意味着只要不出错，loop/1 就一直循环下去，所以进程就不会停止。

`思考：如果把上面代码里 receive 语句的最后一个 _Unsupported -> 分支删掉的话，会发生什么？`

receive 语句里，接受消息时，都要求消息发送方将自己的 Pid 带过来，放到 `From` 变量里，以便我们回复消息给对方。

我们来试试：
``` shell
1> PID = msg_cache:start_one("cache2").
Buff cache2 created! Pid = <0.62.0>
<0.62.0>
2> PID ! {get_length, self()}.
{get_length,<0.60.0>}
3> flush().
Shell got {ok,0}
ok

4> PID ! {pop, self()}.
{pop,<0.60.0>}
5> flush().
Shell got {error,empty}
ok

6> PID ! {push, "msg1", self()}.
{push,"msg1",<0.60.0>}
7> PID ! {push, "msg2", self()}.
{push,"msg2",<0.60.0>}
8> PID ! {push, "msg3", self()}.
{push,"msg3",<0.60.0>}
9> PID ! {get_length, self()}.
{get_length,<0.60.0>}
10> flush().
Shell got ok
Shell got ok
Shell got ok
Shell got {ok,3}
ok

11> PID ! {pop, self()}.
{pop,<0.60.0>}
12> flush().
Shell got {ok,"msg3"}
ok

13> PID ! {get_length, self()}.
{get_length,<0.60.0>}
14> flush().
Shell got {ok,2}
ok
```
**继续往下阅读之前，仔细看一下这个例子，确保你完全理解了这段代码。**

挺厉害的吧？但我们还有两个问题没有解决：
- 没有一个易用易维护的 API。 PID ! {get_length, self()}. 这种调用方式实在有些反人类。
- 没有管理进程。我们调用一次 msg_cache:start_one/1 就启动了一个msg_cache, 但是现在我不知道当前已经启动了几个 msg_cache.

我们来解决这第一个问题，重新整理一下代码：
```erlang
-module(msg_cache).

%% APIs
-export([start_one/1,
         get_name/1,
         get_length/1,
         pop/1,
         set_name/2,
         push/2
        ]).

%% for spawns
-export([loop/1]).

-define(API_TIMEOUT, 3000).

-record(state, {
            name,
            length = 0,
            buff = []
         }).

start_one(BuffName) ->
  Pid = spawn(msg_cache, loop, [#state{name=BuffName}]),
  io:format("Buff ~s created! Pid = ~p~n", [BuffName, Pid]),
  Pid.

%% 加了这几个 API
get_name(CacheID) ->
  call(CacheID, {get_name, self()}).
get_length(CacheID) ->
  call(CacheID, {get_length, self()}).
set_name(CacheID, NewName) ->
  call(CacheID, {set_name, NewName, self()}).
pop(CacheID) ->
  call(CacheID, {pop, self()}).
push(CacheID, Msg) ->
  call(CacheID, {push, Msg, self()}).

%% 由于发送和接受消息的处理方面，各个 API 都差不多，就提取出来专门写个 call 函数，提高代码复用。
call(Pid, Request) ->
  Pid ! Request,
  receive
    Response -> Response
  after ?API_TIMEOUT ->
    {error, api_timeout}
  end.

%% loop 这一部分我们没改动任何代码
loop(State = #state{name = Name, length = Len, buff = Buff}) ->
  receive
    {get_name, From}->
      From ! {ok, Name},
      loop(State);
    {get_length, From}->
      From ! {ok, Len},
      loop(State);
    {set_name, NewName, From} ->
      From ! ok,
      loop(State#state{name = NewName});
    {push, Msg, From} ->
      From ! ok,
      loop(State#state{buff = [Msg | Buff], length = Len + 1});
    {pop, From} ->
      case Buff of
        [] ->
          From ! {error, empty},
          loop(State);
        [TopMsg | Msgs] ->
          From ! {ok, TopMsg},
          loop(State#state{buff = Msgs, length = Len - 1})
      end;
    _Unsupported ->
      erlang:error(io_libs:format("unsupported msg: ", [_Unsupported]) )
  end.
```

再试一下：

```shell
1> PID = msg_cache:start_one("cache_worker_1").
Buff cache_worker_1 created! Pid = <0.62.0>
<0.62.0>
2> msg_cache:get_name(PID).
{ok,"cache_worker_1"}
3> msg_cache:get_length(PID).
{ok,0}
4> msg_cache:pop(PID).
{error,empty}
5> msg_cache:push(PID, "msg1").
ok
6> msg_cache:push(PID, "msg2").
ok
7> msg_cache:get_length(PID).
{ok,2}
8> msg_cache:pop(PID).
{ok,"msg2"}
9> msg_cache:pop(PID).
{ok,"msg1"}
10> msg_cache:pop(PID).
{error,empty}
11> msg_cache:get_length(PID).
{ok,0}
```
还阔以吧？

##### 留个作业
上面那个 "管理进程" 我们没有实现。你来实现它。

我想这么调用：
```shell
%% 启动两个 worker：
1> msg_cache:start_cache_workers(["c_worker_1", "c_worker_2"]).
[<0.62.0>, <0.65.0>]

%% 列出所有 workers, 返回值是个 worker 列表, 元素展示了每个 worker 的 name, pid, 和 length 。
2> CachePidList = msg_cache:list_cache_workers().
[{"c_worker_1", <0.62.0>, 0}, {"c_worker_2", <0.65.0>, 0}]

%% 负载均衡, 会往随机的一个 cache worker 里 push.
%% 注意我这里调用 msg_cache:push 的时候，没有提供某个 cache worker 的 PID
3> ok = msg_cache:push("msg1").
ok
4> ok = msg_cache:push("msg2").
ok
5> CachePidList = msg_cache:list_cache_workers().
[{"c_worker_1", <0.62.0>, 1}, {"c_worker_2", <0.65.0>, 1}]

%% 至于 pop 不用管顺序了，有消息就随便 pop 出一个来。
4> msg_cache:pop().
{ok, "msg1"}
```
提示：
- [erlang:register/2](http://erldocs.com/18.0/erts/erlang.html#register/2 "here be a title. Enjoy") 可以给一个 PID 注册一个名字，以后使用这个 PID 就可以使用这个名字代替。比如
``` erlang
register(msg_cache_manger, Pid).

msg_cache:list_cache_workers() ->
  msg_cache_manger ! get_all_workers.
```

##### 课后必读文章
Erlang 中的错误处理机制, Link、Monitor:
[Errors and Processes](http://learnyousomeerlang.com/errors-and-processes)

# ETS
ETS (Erlang Term Storage) 是设计来存放大量的 Erlang 数据的。跟 ETS 打交道不用消息格式转换，可直接存放 Erlang 数据格式 (erlang 各种数据格式的统称叫做 erlang terms)。
ETS 非常快，访问时间是常数级的，自动帮你解决了多进程访问的各种竞态条件问题，让我们在 Erlang 中做并发编程一身轻松。ETS 是非常优秀的缓存系统，是我们开发中不可或缺的利器之一。这比起用某种流行语言来说，舒服太多[^1]。
ETS 只将数据存储在内存里，如果想保存到磁盘，或者要在多个 Erlang Node 之间共享数据，OTP 基于 ETS 和 DETS 实现了 [mnesia](http://learnyousomeerlang.com/mnesia). 
`NODE: mnesia 只适合用来做缓存，在多个 Node 之间共享少量数据，非常快速。但是并不适合当做数据库存储大量的数据，因为 mnesia 在启动时会加载所有数据到内存里，导致启动缓慢、新节点加入缓慢。并且 mnesia 是强一致性的数据库，其本身并不处理由于集群脑裂导致的不一致性，这可能不太符合你的预期。`

ETS 支持几种数据类型：
- set: set 是普通的 key - value 存储类型，一个 ETS table 里，两个数据的 key 不能相同。重复插入 key 相同的两条数据，后面的那条会覆盖前面的那条。
- ordered_set: 有序的 set 表。
- bag: bag 允许多个 key 相同的数据的存在，但 key, value 都完全相同的数据只能留一个。
- duplicate_bag: 允许多个 key, value 完全相同的数据的存在。

我们来试试 set 类型的 table，这也是最常用的类型。我们创建一个命名表，叫 `users`, 然后插入两条数据：
```shell
1> ets:new(users, [set, named_table]).
users
2> ets:info(users).   %% 注意默认的权限是 protected
[{read_concurrency,false},
 {write_concurrency,false},
 {compressed,false},
 {memory,304},
 {owner,<0.57.0>},
 {heir,none},
 {name,users},
 {size,0},
 {node,nonode@nohost},
 {named_table,true},
 {type,set},
 {keypos,1},
 {protection,protected}]
3> ets:insert(users, {1, <<"Shawn">>, 27}).
true
4> ets:insert(users, {2, <<"Scarlett">>, 25}).
true
5> ets:lookup(users, 1).
[{1,<<"Shawn">>,27}]
6> ets:lookup(users, 2).
[{2,<<"Scarlett">>,25}]
7> ets:info(users).
[{read_concurrency,false},
 {write_concurrency,false},
 {compressed,false},
 {memory,332},
 {owner,<0.57.0>},
 {heir,none},
 {name,users},
 {size,2},
 {node,nonode@nohost},
 {named_table,true},
 {type,set},
 {keypos,1},
 {protection,protected}]
8>
```
注意上边的示例里：
- 创建 ETS table 时给了两个 Options 参数：[set, named_table]。set 是指定创建 set 类型的表，named_table 是创建命名表，命名为 `users `，后面可以用这个表名来引用。
- 插入数据 `{1, <<"Shawn">>, 27}` 和 `{2, <<"Scarlett">>, 25}` 时，两个 tuple 的第一项就是默认的 key，tuple 里其他项都是 values。如果你想用其他的项作为 key，可以在  ets:new 的时候，指定 `{keypos, Pos}` 参数，设置 key 在 tuple 中的位置。

ETS 表的其他类型你可以自己试验一下。

**需要注意的是：**
- ETS 表里的任何数据都不参加 GC
- ETS 表有自己的 `owner` 进程，默认情况下，创建表的那个进程就是 ETS table 的 owner。如果 owner 进程挂了，ETS 表也就被释放了。我们上边的例子里，erlang shell 进程就是 `user` table 的 owner。
- ETS 表也是有访问权限的，默认是 `protected`:
  * public：任何人可以读写这张表。
  * protected: owner 可以读写，但其他进程只能读。
  * private：只有 owner 可以读写。别的进程无法访问。

由于 ETS 表非常高效，一般情况下我们都直接使用 `public`，然后设置 `{read_concurrency, true}` 或 `{write_concurrency,true}` 选项来提高并发读或写的效率，在写一个管理模块来直接访问 ets 表，让什么封装什么设计模式都去 shi。

# OTP
OTP 已经失去了字面意思，基本上指的就是 Erlang 生态环境的官方部分。Erlang 世界的组成是这样的：
- Erlang 以及 Elixir 等语言。
- 工具和函数库，包括 erlang runtime，kernel，stdlib(像 lists 这种的官方库), sasl, 还有像 ETS，dbg 之类的很多。
- 系统设计原则, 包括本章要讲的一众 Behaviors。是一堆应用于并发世界的设计模式，他们包含了解决通用问题的通用代码。
- 开源社区生态环境，包括各种开源软件和社区。

OTP 指的是前三个，Elixir 的话还不大算。

Erlang 的逻辑是，架构的设计应该由有经验的人负责，由专家做好基础代码框架，解决好最困难的问题。而使用者只需要写自己的逻辑代码。这就是 OTP behaviors，他们已经在通信、互联网领域，经历了几十年的战火考验。

本文要讲的有三个：
- gen_server
- application
- supervisor

本章只讲解 gen_server。 application 和 supervisor 放到后面 Hello World 工程里讲解。

**gen_server** 要解决的问题，就是我们上面那个 msg_cache 面临的问题：怎样做一个服务来响应用户的请求。

我们之前写的代码很短，可以工作，但是很多东西都没有考虑。比如请求者如果同时收到来自服务端的两个 Response 的话，不知道是对应哪个请求的：
```erlang
%% 服务端：
    {get_name, From}->
      From ! {ok, Name},
      loop(State);
    {get_length, From}->
      From ! {ok, Len},
      loop(State);

%% 客户端：
    ServerPID ! {get_length, self()},   %% 客户端连续调用了两次
    ServerPID ! {get_length, self()},  
    receive
      {ok, Len} ->  %% 你知道这次匹配到的消息，是上面哪次调用的回复吗？
         success;
      _ ->
         failed
    end.
```
上面代码里连续调用了两次 {get_length}, 但是由于发送消息是异步的，消息通过网络回来，你并不能确定第一次收到的回复就是第一次调用产生的。

这个问题可以加一个随机生成的 RequestID 的字段来解决，客户端发送请求消息的时候带 RequestID 过去，服务端返回的时候再传回来。客户端通过匹配 RequestID，就能知道当前的回复是对应的哪个请求。

但这种需求其实是通用的，你现在写 msg_cache 用得到，改天写其他代码也一样用得到。另外我们也没有过多考虑异常的情况：如果程序崩溃了怎么办？发送消息怎么知道对方是不是还活着？

诸如此类的问题应该由专家来解决，所以我们有了 `gen_server`.
gen_server 的模板是这样的：
```erlang
-module(gen_server_demo).
-behaviour(gen_server).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).
%%%% %%%% %%%% %%%% %%%% 
%%%% 这是给客户端调用的接口部分
%%%% %%%% %%%% %%%% %%%% 
%% 启动一个服务，后台会启动一个 erlang process, 并进入 loop 函数, 回想一下我们实现 msg_cache 时写的那个 loop/1.
%% 但是这个 loop 函数属于通用部分的代码，是由 OTP 官方实现的，所以代码不在这里，在 OTP 代码的 lib/stdlib/src/gen_server.erl 里。
start_link() ->
    %% gen_server:start_link 启动 process, 然后将 process 注册在当前
    %% node 上，注册名字就是当前 Module 名：gen_server_demo
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%% %%%% %%%% %%%% %%%% 
%%%% 这是 gen_server 发生某事件时的回调函数部分
%%%% %%%% %%%% %%%% %%%%

%% gen_server:start_link 被调用，服务启动时，回调 init/1
init([]) ->
    {ok, #state{}}.

%% gen_server:call 被调用。gen_server:call 是“同步”调用，调用方可以设置一个超时时间。
%% 返回值里的 Reply 是返回给调用者的内容。
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% gen_server:cast 被调用。gen_server:cast 是“异步”调用。
%% 调用者一般是想发一个消息给我们的 gen_server，然后继续做自己的事情，他不想收到来自 gen_server 的回复。
handle_cast(_Msg, State) ->
    {noreply, State}.

%% gen_server 进程收到一个普通 erlang 消息：一个不是通过 gen_server:call 和 gen_server:cast 发来的消息。
handle_info(_Info, State) ->
    {noreply, State}.

%% 上面的三个函数 handle_call, handle_cast, handle_info
%%   都可以返回一个 {stop, Reason, State}，这样的话 gen_server 会退出。
%%   但退出之前，可能会回调 terminate(_Reason, _State)。


%% gen_server 将要退出时，回调 terminate/2.
%% 注意
%% 1) 要想 terminate 在 gen_server 退出前被回调，gen_server 必须捕获退出信号：
%%    需要在 init 回调里，加这么一行：process_flag(trap_exit, true).
%% 2) 有几个特定的 Reason 被认为是正常退出：normal, shutdown, or {shutdown,Term}，
%%    其他的 Reason，sasl 是会报错打日志的。
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```
gen_server 真正的进程代码在 OTP 库里，运行 start_link()，gen_server 就在后台跑起来了。你需要实现的只是这个模板里的各个回调函数，将你的业务逻辑放到这些回调里。

**仔细看一下上面的 gen_server 模板和注释，确保你能完全理解。**

我不想重新实现之前的 msg_cache，一点都不酷。我们重新写个其他的，让你对 Erlang 程序的基本设计理念有更深的印象。

我们要实现一个多用户聊天的程序：
- 用户能够查询在线的其他用户。
- 用户之间能够聊天。
- 要容易扩展，因为后面我们的 Client 会通过TCP、WebSocket 等连接上来，不会是 Erlang 写的 Client。
- 要容易伸缩，因为我们业务发展很快，用户量会越来越大，我们希望程序能很容易的部署在多台服务器上。

先来设计我们程序的架构：
![chat_server-2.png](https://upload-images.jianshu.io/upload_images/2997299-7100392cb582c4ab.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
- 每个 client 连接上来，都会启动一个新的 Process，叫做 ChatServer.
- ChatServer 负责维护这个 Client 的 TCP 连接。
- Route 是一个Module，它提供了数据库的管理，数据库里维护了从 User 到其 ChatServer 的 PID 的映射关系。

注意我们的设计思想：
- 为每一个连接上来的请求启动一个 Erlang 进程 "ChatServer"，不要担心进程个数，百万也没问题。
- 两个用户之间的消息传递，体现在服务端就是两个 "ChatServer" 之间的 Erlang 消息传递。
- Route 部分只是一个 Module，不是进程。每一个 ChatServer 调用 Route 里的代码的时候，执行过程其实是在每个 ChatServer 进程内部的。这样我们就避免了集中向一个进程发送消息带来的瓶颈。我们把这种瓶颈的处理留给了 ETS 来解决。
- 如何伸缩？ChatServer 在不在同一个服务器上没什么关系。`ChatServerPID ！{send, Msg}` 会将消息发送到ChatServerPID，即使 ChatServerPID 在远端的服务器上。分布式部署的时候，这行代码根本不用改，你要做的仅仅是添加一个新的 Erlang Node。分布式 Erlang 后面还要讲。
- 如何扩展？ETS 使用 Route Module 管理，为的就是当以后换用其他的缓存数据库的时候简单一些。我们设想后面为了做分布式集群，要用 mnesia 替代 ETS，只需要写一个新的 Route Module，内部改用 mnesia 存储，然后替换线上已经加载的老的 Route Module。线上系统都不用停止，客户端的连接一个都不会断！

`你现在能否体会到 Erlang 的实用主义呢？完全没废话，就是解决问题！`

Client 部分我们现在不做，让前端的同学帮我们实现。但假设我们的前端程序员还没到岗，所以我们可以先放着 WebSocket 部分后面再做。但有两个过程必须现在实现：
- 当 Client 登录时，我们需要使用 Route 注册 user 所在的 ChatServer 的 PID。
- 当 Client 发消息时，我们需要使用 Route 查找对方的 ChatServer 的 PID。

首先我们来定义我们的消息协议。我们的消息体内包含几部分，发送者ID，接收者ID，以及消息内容：
```erlang
-record(msg, {
  from_userid,
  to_userid,
  payload
}).
```

接下来让我们来实现 Route 模块，实现数据库创建，注册，查找与注销功能：
```erlang
-module(route).
-export([ensure_db/0,
         lookup_server/1,
         register_server/2,
         unregister_server/1]).

ensure_db() ->
  case ets:info(servers) of
    undefined ->
      %% 为了演示方便，我们启动一个临时进程来创建 ETS 表，
      %% 如果直接在 erlang shell 里创建ETS的话，出错时 shell 的崩溃连带着我们的ETS也丢了。
      %% 当然线上系统不会这么做。
      spawn(fun() -> ets:new(servers, [named_table, public]), receive after infinity->ok end end);
    _ -> ok
  end.

lookup_server(UserID) ->
  case ets:lookup(servers, UserID) of
    [{UserID, ServerID}] -> {ok, ServerID};
    _ -> {error, no_server}
  end.

register_server(UserID, ServerID) ->
  ets:insert(servers, {UserID, ServerID}).

unregister_server(UserID) ->
  ets:delete(servers, UserID).
```

接下来实现我们的 ChatServer:
```erlang
-module(chat_server).
-behaviour(gen_server).
%% state 保存用户的 userid，以及 client 端连上来的 socket.
-record(state, {
  userid,
  socket
}).

%% 后面当一个新连接连接到服务器的时候，我们会调用 start_link 启动一个新的 ChatServer 为之服务。
%% 注意这里使用的是 gen_server:start_link/3，没有注册进程名，我们直接使用 PID. 因为我们要启动很多个 ChatServer。
start_link(UserID, Socket) ->
  {ok, ServerID}  = gen_server:start_link(?MODULE, [UserID, Socket], []),
  ServerID.

%% 在 init 回调里注册用户的 ChatServer。
%% 注意我们捕获了 exit signal, 以便程序退出的时候回调 terminate/2. 
%% 我们在 terminate/2 里取消注册。
init([UserID, Socket]) ->
    process_flag(trap_exit, true),
    route:register_server(UserID, self()),
    {ok, #state{userid=UserID, socket=Socket}}.

%% 如果我们的 ChatServer 收到一条来自 Socket 的消息，它会收到一条类似 {tcp, Sock, Data} 的普通消息。
%% 我们需要在 handle_info 里处理，转发给对方的 ChatServer。
handle_info({tcp, #msg{to_userid = ToUserID, payload = Payload} = Msg}, State) ->
  io:format("Chat Server(User: ~p) - received msg from tcp client, Msg: ~p~n",[State#state.userid, Msg]),
  case route:lookup_server(ToUserID) of
    {error, Reason} ->
      io:format("Chat Server(User: ~p) - cannot forward to Chat Server(User: ~p): ~p~n",
          [State#state.userid, ToUserID, Reason]);
    {ok, TargetServerID} ->
      io:format("Chat Server(User: ~p) - forward msg to Chat Server(User: ~p), Payload: ~p~n",
        [State#state.userid, ToUserID, Payload]),
      ok = gen_server:call(TargetServerID, {send, Msg})
  end,
  {noreply, State};

%% 我们的 ChatServer 收到一条来自对端 ChatServer 的转发请求
handle_call({send, #msg{payload = Payload}}, _From, State) ->
  io:format("Chat Server(User: ~p) - deliver msg to tcp client, Payload: ~p~n",
    [State#state.userid, Payload]),
  send_to_client_via_tcp(State#state.socket, Payload),
  {reply, ok, State};

%% Socket 部分我们没有实现，暂时就简单打印一下
send_to_client_via_tcp(_Socket, Payload) ->
  %gen_tcp:send(_Socket, Payload),
  io:format("Sent To Client: ~p~n",[Payload]).
```
完工了！我们测试一下：
```shell
1> c(chat_server).
{ok,chat_server}
2> c(route).
{ok,route}

%% 现在模拟系统启动时，初始化 DB 的过程。
%% 后续这个会在启动代码里写。
3> route:ensure_db().
<0.73.0>

%% 现在我们模拟一个新的用户登录上来，启动新的 ChatServer 的过程。
%% 后续这个过程当然是由 WebSocket 模块调用。
4> ServerIDUser1 = chat_server:start_link(<<"user1">>, fake_socket).
<0.75.0>
5> ServerIDUser2 = chat_server:start_link(<<"user2">>, fake_socket).
<0.77.0>

%% 我们来做一个 #msg{} 消息体。
%% 后续我们应该在收到 socket 上来的消息解析成功之后，打包一个 #msg{} 消息体。
6> rr("chat_protocol.hrl").
[msg]
7> Msg = #msg{from_userid= <<"user1">>, to_userid = <<"user2">>, payload = <<"hello?">>}.
#msg{from_userid = <<"user1">>,to_userid = <<"user2">>,
     payload = <<"hello?">>}


%% 模拟从 socket 收到消息的过程。
8> ServerIDUser1 ! {tcp, Msg}.
Chat Server(User: <<"user1">>) - received msg from tcp client, Msg: {msg,
                                                                     <<"user1">>,
                                                                     <<"user2">>,
                                                                     <<"hello?">>}
{tcp,#msg{from_userid = <<"user1">>,to_userid = <<"user2">>,
          payload = <<"hello?">>}}
Chat Server(User: <<"user1">>) - forward msg to Chat Server(User: <<"user2">>), Payload: <<"hello?">>
Chat Server(User: <<"user2">>) - deliver msg to tcp client, Payload: <<"hello?">>
Sent To Client: <<"hello?">>
9>
```
我们看到服务端的路由已经走通了，接下来只要写一个 web socket 模块，listen 在某个端口，当有连接请求时，像上面第 4、第 5 行一样调用 chat_server:start_link/2 就行了。当然 send_to_client_via_tcp 也要改为真正往 socket 发送消息。

完整代码：
https://github.com/terry-xiaoyu/learn-erlang-in-30-mins/tree/master/chat

一个完整的线上演示：
(即将上线)

**书接下文**：[30 分钟学 Erlang (三)](https://www.jianshu.com/p/bbaf695ec167)
[^1]: Golang 里你需要自己找多线程安全的 maps 库，写并发没有安全感。Golang 官方也没有下文要说到的 OTP 里提供的各种 Behavior，代码写起来天马行空最后一团糟。然后又不能支持函数式的 pattern matching 等写法... 总之用 golang 写代码从来不会给人愉快的感觉。流行是流行的，但那叫“普通”吧？第一次这么吐槽 golang，但这篇是 erlang 的教程，应该不算过分吧。等到写 go 的时候我再来吐槽 erlang 。我是不会写 go 的 ...