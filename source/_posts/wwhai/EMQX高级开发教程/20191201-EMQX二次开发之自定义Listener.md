---
title: EMQX二次开发之自定义Listener
date:  2019-12-01 19:06:03
index_img: /static/6.jpg
tags: 
- Erlang
- EMQX

categories: 
- EMQX高级开发教程

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文主要讲解如何在EMQX中添加自定义Listener
<!-- more -->

## 1.简介

![image-20191201190852642](/uploads/image-20191201190852642.png)

​        EMQX目前支持的Listener(监听器)如上图所示:

- MQTT监听器
- SSL监听器
- WebSocket监听器
- Http监听器
- WSS监听器

​        这些监听器构成了EMQX的协议核心入口,客户端首先链接监听器,然后把数据传输给EMQX内核,进行转发或者处理.简而言之,监听器就是监控某个端口的一个进程,用来提供某种数据服务.

​        了解了上面的基础以后,我们会有这样的疑问:目前看起来仅仅支持MQTT协议和HTTP协议,如果我自己有个新的协议接入,是不是就不行了?答案是:没错,EMQX并没有支持更多的协议,所以我们的自定义协议不可能接入进来,但是难道没有办法吗?EMQX的官方文档里面也没有提这个,于是我自己稍微研究了一下,总结出来一个办法:我们的自定义协议完全可以融入进EMQX内核,只不过需要自定义二次开发.说到这里其实就有门槛了,首先你得熟悉点Erlang语言,然后就是熟悉一下EMQX的源码.

​         好了,我们直接开始讲如何开发自定义监听器,或者说是如何把自定义协议接入进EMQX,让EMQX成为我们的一个私有化协议服务器.

## 2.准备工作

​        下面的工作都基于你已经准备好Erlang/OTP环境,注意一下:必须是22以上的版本 .

1. 克隆EMQX源码

   ```shell
   git clone https://github.com/wwhai/emqx.git
   ```

2. 克隆emqx-rel源码

   ```
   git clone https://github.com/wwhai/emqx-rel.git
   ```

## 3.代码结构

1. emqx的代码结构如图所示:

![image-20191201191908028](/uploads/image-20191201191908028.png)

2. emqx-rel的代码架构如下:

   ![image-20191201192054291](/uploads/image-20191201192054291.png)



​        熟悉Erlang开发的人是不是看起来很眼熟?没错,其实就是一个标准的rebar项目,细节我就不说了,首先我们来看下EMQX项目,emqx-rel项目最后会分析.

## 4.EMQX源码简单解析

​        我们切换到emqx项目下的src目录,文件结构如下:

![image-20191201192303160](/uploads/image-20191201192303160.png)

​        其他的文件我暂时没有去研究,我们找到我们的目标:listener,进来看下源码内容:

```erlang

-module(emqx_listeners).

-include("emqx_mqtt.hrl").

%% APIs
-export([start/0
    , restart/0
    , stop/0
]).

-export([start_listener/1
    , start_listener/3
    , stop_listener/1
    , stop_listener/3
    , restart_listener/1
    , restart_listener/3
]).

-type(listener() :: {esockd:proto(), esockd:listen_on(), [esockd:option()]}).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

%% @doc Start all listeners.
-spec(start() -> ok).
start() ->
    Config = emqx:get_env(listeners, []),
    lists:foreach(fun start_listener/1, Config).

-spec(start_listener(listener()) -> {ok, pid()} | {error, term()}).
start_listener({Proto, ListenOn, Options}) ->
    io:format("Proto:~p, ListenOn:~p, Options:~p", [Proto, ListenOn, Options]),
    StartRet = start_listener(Proto, ListenOn, Options),
    case StartRet of
        {ok, _} -> io:format("Start ~s listener on ~s successfully.~n",
            [Proto, format(ListenOn)]);
        {error, Reason} ->
            io:format(standard_error, "Failed to start ~s listener on ~s - ~p~n!",
                [Proto, format(ListenOn), Reason])
    end,
    StartRet.

%% Start MQTT/TCP listener
-spec(start_listener(esockd:proto(), esockd:listen_on(), [esockd:option()])
        -> {ok, pid()} | {error, term()}).


start_listener(tcp, ListenOn, Options) ->
    start_mqtt_listener('mqtt:tcp', ListenOn, Options);


%% Trap
start_listener(tcp, ListenOn, Options) ->
    start_trap_listener('trap:tcp', ListenOn, Options);


%% Start MQTT/TLS listener
start_listener(Proto, ListenOn, Options) when Proto == ssl; Proto == tls ->
    start_mqtt_listener('mqtt:ssl', ListenOn, Options);

%% Start MQTT/WS listener
start_listener(Proto, ListenOn, Options) when Proto == http; Proto == ws ->
    start_http_listener(fun cowboy:start_clear/3, 'mqtt:ws', ListenOn,
        ranch_opts(Options), ws_opts(Options));

%% Start MQTT/WSS listener
start_listener(Proto, ListenOn, Options) when Proto == https; Proto == wss ->
    start_http_listener(fun cowboy:start_tls/3, 'mqtt:wss', ListenOn,
        ranch_opts(Options), ws_opts(Options)).

start_mqtt_listener(Name, ListenOn, Options) ->
    SockOpts = esockd:parse_opt(Options),
    esockd:open(Name, ListenOn, merge_default(SockOpts),
        {emqx_connection, start_link, [Options -- SockOpts]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2019-11-29
%% Trap listener
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_trap_listener(Name, ListenOn, Options) ->
    io:format("start_trap_listener: Name :~p ListenOn:~p Options:~p ", [Name, ListenOn, Options]),
    SockOpts = esockd:parse_opt(Options),
    esockd:open('trap:tcp', ListenOn, merge_default(SockOpts),
        {emqx_trap_connection, start_link, [Options -- SockOpts]}).


start_http_listener(Start, Name, ListenOn, RanchOpts, ProtoOpts) ->
    Start(Name, with_port(ListenOn, RanchOpts), ProtoOpts).
%% For websocket mqtt path
mqtt_path(Options) ->
    proplists:get_value(mqtt_path, Options, "/mqtt").

ws_opts(Options) ->
    WsPaths = [{mqtt_path(Options), emqx_ws_connection, Options}],
    Dispatch = cowboy_router:compile([{'_', WsPaths}]),
    ProxyProto = proplists:get_value(proxy_protocol, Options, false),
    #{env => #{dispatch => Dispatch}, proxy_header => ProxyProto}.

ranch_opts(Options) ->
    NumAcceptors = proplists:get_value(acceptors, Options, 4),
    MaxConnections = proplists:get_value(max_connections, Options, 1024),
    TcpOptions = proplists:get_value(tcp_options, Options, []),
    RanchOpts = #{num_acceptors => NumAcceptors,
        max_connections => MaxConnections,
        socket_opts => TcpOptions},
    case proplists:get_value(ssl_options, Options) of
        undefined -> RanchOpts;
        SslOptions -> RanchOpts#{socket_opts => TcpOptions ++ SslOptions}
    end.

with_port(Port, Opts = #{socket_opts := SocketOption}) when is_integer(Port) ->
    Opts#{socket_opts => [{port, Port} | SocketOption]};
with_port({Addr, Port}, Opts = #{socket_opts := SocketOption}) ->
    Opts#{socket_opts => [{ip, Addr}, {port, Port} | SocketOption]}.

%% @doc Restart all listeners
-spec(restart() -> ok).
restart() ->
    lists:foreach(fun restart_listener/1, emqx:get_env(listeners, [])).

-spec(restart_listener(listener()) -> any()).
restart_listener({Proto, ListenOn, Options}) ->
    restart_listener(Proto, ListenOn, Options).

-spec(restart_listener(esockd:proto(), esockd:listen_on(), [esockd:option()]) -> any()).
restart_listener(tcp, ListenOn, _Options) ->
    esockd:reopen('mqtt:tcp', ListenOn);
restart_listener(Proto, ListenOn, _Options) when Proto == ssl; Proto == tls ->
    esockd:reopen('mqtt:ssl', ListenOn);
restart_listener(Proto, ListenOn, Options) when Proto == http; Proto == ws ->
    cowboy:stop_listener('mqtt:ws'),
    start_listener(Proto, ListenOn, Options);
restart_listener(Proto, ListenOn, Options) when Proto == https; Proto == wss ->
    cowboy:stop_listener('mqtt:wss'),
    start_listener(Proto, ListenOn, Options);
restart_listener(Proto, ListenOn, _Opts) ->
    esockd:reopen(Proto, ListenOn).

%% @doc Stop all listeners.
-spec(stop() -> ok).
stop() ->
    lists:foreach(fun stop_listener/1, emqx:get_env(listeners, [])).

-spec(stop_listener(listener()) -> ok | {error, term()}).
stop_listener({Proto, ListenOn, Opts}) ->
    StopRet = stop_listener(Proto, ListenOn, Opts),
    case StopRet of
        ok -> io:format("Stop mqtt:~s listener on ~s successfully.~n",
            [Proto, format(ListenOn)]);
        {error, Reason} ->
            io:format(standard_error, "Failed to stop mqtt:~s listener on ~s - ~p~n.",
                [Proto, format(ListenOn), Reason])
    end,
    StopRet.

-spec(stop_listener(esockd:proto(), esockd:listen_on(), [esockd:option()])
        -> ok | {error, term()}).
stop_listener(tcp, ListenOn, _Opts) ->
    esockd:close('mqtt:tcp', ListenOn);
stop_listener(Proto, ListenOn, _Opts) when Proto == ssl; Proto == tls ->
    esockd:close('mqtt:ssl', ListenOn);
stop_listener(Proto, _ListenOn, _Opts) when Proto == http; Proto == ws ->
    cowboy:stop_listener('mqtt:ws');
stop_listener(Proto, _ListenOn, _Opts) when Proto == https; Proto == wss ->
    cowboy:stop_listener('mqtt:wss');
stop_listener(Proto, ListenOn, _Opts) ->
    esockd:close(Proto, ListenOn).

merge_default(Options) ->
    case lists:keytake(tcp_options, 1, Options) of
        {value, {tcp_options, TcpOpts}, Options1} ->
            [{tcp_options, emqx_misc:merge_opts(?MQTT_SOCKOPTS, TcpOpts)} | Options1];
        false ->
            [{tcp_options, ?MQTT_SOCKOPTS} | Options]
    end.

format(Port) when is_integer(Port) ->
    io_lib:format("0.0.0.0:~w", [Port]);
format({Addr, Port}) when is_list(Addr) ->
    io_lib:format("~s:~w", [Addr, Port]);
format({Addr, Port}) when is_tuple(Addr) ->
    io_lib:format("~s:~w", [inet:ntoa(Addr), Port]).


```



​        我们来分析一下代码:首先start函数是入口,启动以后,通过get_env函数来拿到配置参数文件,分别启动不同协议支持的listener.

```erlang
%% 模块入口函数
start() ->
    Config = emqx:get_env(listeners, []),
    lists:foreach(fun start_listener/1, Config).

```



​        其中我们重点来看下`stasrt_listener/1`函数:

```erlang
%% 启动监听器
start_listener({Proto, ListenOn, Options}) ->
    StartRet = start_listener(Proto, ListenOn, Options),
    case StartRet of
        {ok, _} -> io:format("Start ~s listener on ~s successfully.~n",
            [Proto, format(ListenOn)]);
        {error, Reason} ->
            io:format(standard_error, "Failed to start ~s listener on ~s - ~p~n!",
                [Proto, format(ListenOn), Reason])
    end,
    StartRet.
```



其中`emqx:get_env(listeners, [])`这里取到了配置参数,然后封装成`{Proto, ListenOn, Options}`,第一个是协议的名字,第二个是监听的端口和IP,第三个是esockd(EMQX内部使用的一个异步连接库,大家可以去github上看相关资料)连接参数.

下面的代码是启动系统自己的mqtt监听器:

```erlang
start_mqtt_listener(Name, ListenOn, Options) ->
    SockOpts = esockd:parse_opt(Options),
    esockd:open(Name, ListenOn, merge_default(SockOpts),
        {emqx_connection, start_link, [Options -- SockOpts]}).
```

其实到这里我们已经熟悉了EMQX的监听器启动过程:

1. 通过get_env拿到参数;
2. 调用stasrt_listener函数启动.

到这里是不是我们就可以照葫芦画瓢来自己实现个监听器加进去?答案是Yes,完全可以,下面这段代码就是我自己的监听器:

```erlang
%% Trap
    
%% ........省略其他部分.....
start_listener(trap, ListenOn, Options) ->
    start_trap_listener('trap:tcp', ListenOn, Options);
    
%% ........省略其他部分.....
start_trap_listener(Name, ListenOn, Options) ->
    io:format("start_trap_listener: Name :~p ListenOn:~p Options:~p ", [Name, ListenOn, Options]),
    SockOpts = esockd:parse_opt(Options),
    esockd:open('trap:tcp', ListenOn, merge_default(SockOpts),
        {emqx_trap_connection, start_link, [Options -- SockOpts]}).
    
%% ........省略其他部分.....

```

​        其中trap是我自定义的协议名字,通过esockd:open函数启动.`emqx_trap_connection`是我们自定义协议的处理模块,其实就是个很普通的Erlang 模块:

```erlang
-module(emqx_trap_connection).

-include("emqx.hrl").
-include("emqx_mqtt.hrl").
-include("logger.hrl").
-include("types.hrl").

-logger_header("[TRAP]").
-export([start_link/2, init/2]).

start_link(Transport, ListenSocket) ->
    {ok, spawn_link(?MODULE, init, [Transport, ListenSocket])}.

init(Transport, ListenSocket) ->
    case Transport:wait(ListenSocket) of
        {ok, NewSock} ->
            loop(Transport, NewSock);
        Error -> Error
    end.

loop(Transport, RemoteSocket) ->
    case Transport:recv(RemoteSocket, 0) of
        {ok, BinData} ->
            {ok, PeerName} = Transport:peername(RemoteSocket),
            io:format("BinData from ~s: and data is : ~p ~n", [esockd_net:format(peername, PeerName), BinData]),
            Transport:send(RemoteSocket, <<"OK">>),
            loop(Transport, RemoteSocket);
        {error, Reason} ->
            io:format("TCP Error: ~s~n", [Reason]),
            {stop, Reason}
    end.

```

 start_link以后通过init函数开启一个监听服务,到此为止,我们的监听器已经适配成功.

​        可能有的小伙伴比较开心,终于搞定了!但是我还没说Ok呢,到这里完了吗?这样就能跑起来了吗?答案当然是不能,前面提到的配置在哪里?还没讲呢,这样仅仅是把监听器的Erlang部分处理好了,但是还没有处理配置部分.接下来我们处理配置文件.

## 5.自定义协议配置

​        EMQX的配置文件是通过cuttlefish这个框架来做映射处理的,因此我们得熟悉一下cuttlefish.因为上一篇文章已经大概讲了,这里不做赘述,我们直接看相关代码即可.

​        首先找到priv目录下的schema文件:

![image-20191201194121823](/uploads/image-20191201194121823.png)



​        找到大概1780行:

![image-20191201194246035](/uploads/image-20191201194246035.png)

​        为甚看这里?主要这里就是用来处理自定义协议配置的,仔细观察发现这行关键代码:

```erlang
cuttlefish_variable:filter_by_prefix("listener.tcp", Conf)
```

​        表示cuttlefish筛选listener.tcp开头的配置,然后解析出来供给get_env函数使用,上面的函数抽象一下就是如下形式:

```erlang
cuttlefish_variable:filter_by_prefix("listener.自定义协议名字", Conf)
```

​        我们的案例是自定义的trap协议,所以我直接加了trap协议配置的处理函数:

```erlang
cuttlefish_variable:filter_by_prefix("listener.trap", Conf)
```

​        cuttlefish解决的问题是:让用户看得懂的配置,所以我们目前仅仅是配置了开发阶段的,还没有配置用户的入口.接下来我们配置用户的入口.

## 6.用户配置

1. 打开etc目录下的emqx.conf文件

​        ![image-20191201201456984](/uploads/image-20191201201456984.png)

2. 加入自定义协议的配置

   ​        直接拉到最下面,加入以下内容:

   ```properties
   ##--------------------------------------------------------------------
   ## Trap listener
   ##--------------------------------------------------------------------
   listener.trap.external = 0.0.0.0:2600
   listener.trap.external.acceptors = 8
   listener.trap.external.max_connections = 1024000
   listener.trap.external.max_conn_rate = 1000
   listener.trap.external.active_n = 100
   listener.trap.external.zone = external
   listener.trap.external.access.1 = allow all
   listener.trap.external.backlog = 1024
   listener.trap.external.send_timeout = 15s
   listener.trap.external.send_timeout_close = on
   listener.trap.external.nodelay = true
   listener.trap.external.reuseaddr = true
   
   ```

   ​        可以看出我们这里的协议名字和cuttlefish里面配置的一样:trap.到此为止,我们总算是完成了所有的基本配置操作,接下来我们开始编译.

   ## 7.EMQX编译

   回到我们刚开始拉下来的emqx-rel项目,打开rebar.config文件,找到15行左右:

   ![image-20191201202205235](/uploads/image-20191201202205235.png)

   指定一个仓库,我自己指定的是:

   ```shell
   {emqx,{git,"https://github.com/emqx/emqx",{branch,"master"}}}
   ```

   然后把之前修改过的emqx 源码push到上面的仓库,表示是emqx从这里构建,不然默认是emqx的官方仓库.

   最后执行下面的命令:

   ```shell
   make
   ./_build/emqx/rel/emqx/bin/emqx console
   ```

   如果没有问题,你可以看见自己的监听器:

   ![image-20191201190852642](/uploads/image-20191201190852642.png)



## 8.总结

1. 如何获得源码
2. 如何修改emqx源码
3. 如何配置cuttlefish
4. 如何编译自定义的emqx









