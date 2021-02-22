---
title: EMQX核心连接器-Esockd框架的使用
date:  2019-12-30 15:36:58
index_img: /static/22.jpg
tags: 
- 服务器
- emqx

categories: 
- Erlang

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文主要讲EMQX开源出来的连接框架Esockd的基本使用
<!-- more -->

## 1.概述

我们用Erlang开发TCP应用的时候,要么是用Erlang自己的库,要么是用一些用Erlang实现的三方库,比如lib_chan等.经过两个月的学习,我大概总结了一下,不能说是哪种做法不对或者对,一般都是按照实际业务选择框架,况且Erlang对TCP天然支持非常好,所以这里仁者见仁.

而今天我给大家带来的也是一个三方公司实现的高性能TCP框架:Esockd.

## 2.Esockd基础

说起这个esockd可能很多朋友还不知道,但是很多玩物联网的朋友可能听过EMQ这个物联网消息服务器,而Esockd框架就是EMQX的核心连接器实现框架.官方仓库在此;https://github.com/emqx/esockd.

下面我就简单解读一下官方的Demo实现.

## 3.一个简单的TCP服务器的实现

这是官方给出的最简单的一个Demo:

```erlang
-module(echo_server).

-export([start_link/2, init/2]).

start_link(Transport, Sock) ->
    {ok, spawn_link(?MODULE, init, [Transport, Sock])}.

init(Transport, Sock) ->
    case Transport:wait(Sock) of
        {ok, NewSock} ->
            loop(Transport, NewSock);
        Error -> Error
    end.

loop(Transport, Sock) ->
    case Transport:recv(Sock, 0) of
        {ok, Data} ->
            {ok, Peername} = Transport:peername(Sock),
            Transport:send(Sock, Data),
            loop(Transport, Sock);
        {error, Reason} ->
            io:format("TCP Error: ~s~n", [Reason]),
            {stop, Reason}
    end.
```

我们解读一下代码:

1. 启动

   ```erlang
   start_link(Transport, Sock) ->
       {ok, spawn_link(?MODULE, init, [Transport, Sock])}.
   ```

   上面这段代码用到了一个高级函数:spawn_link,这个函数的作用是把当前模块变成一个系统进程.

   ```erlang
   Transport:wait(Sock)
   ```

   上述代码是大家比较熟悉的等待连接的过程.

2. 循环接受消息

   ```erlang
   Transport:recv/2
   ```

   上述函数是接收消息,这个也何标准的库类似.有数据以后会返回元组:{ok, Data}.

我们简单分析了一下esockd的demo发现好像和标准库区别不大,接下来我们再来看几个高级特性.

## 4.API解读

1. 开启一个监听器

   ```erlang
   esockd:open(echo, 5000, [{tcp_options, [binary, {reuseaddr, true}]}],
               {echo_server, start_link, []}).
   ```

   ​        上述代码表示开启一个名字为echo的服务器,监听5000端口,后面的元组则是大家熟悉的TCP Socket连接参数.

   ```erlang
   esockd:open(echo, {"127.0.0.1", 6000}, [{tcp_options, [binary, {reuseaddr, true}]}],
               {echo_server, start_link, []}).
   ```

   ​        上述代码不一样的是出现了一个元组:`{"127.0.0.1", 6000}`,这个看起来也很熟悉,就是IP地址过滤,只允许本地连接.

2. ACL权限控制

   ```erlang
   esockd:allow({echo, 5000}, all).
   esockd:allow({echo, 5000}, "192.168.0.1/24").
   esockd:deny({echo, 5000}, all).
   esockd:deny({echo, 5000}, "10.10.0.0/16").
   ```

   ​        上述代码是ACL权限控制,表示允许的网段/IP,其中all表示连接所有.

## 5.服务器实战

​        学玩上面的基础知识,我们只需要稍微把之前写的标准库实现的TCP服务器改动一下,就可实现一个高性能的服务器.

​       下面是之前提到的trap协议服务器端的实现主要代码:

```erlang
-module(trap_tcp_server).
-behaviour(gen_server).
-export([start_link/2, start/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(trap_client_info, {id, client_id, ip, socket, auth = false, mode}).
%% Connection info,保存进进程
-record(trap_connection_info, {id, client_id, ip, transport, socket, auth = false, mode}).

%% ETS 表名
-define(CLIENT_TABLE, trap_connection_table).
%% 等待认证事件5S
-define(EXPIRE_TIME, 3000).


start(Port) when is_integer(Port) ->
  %% Start ets
  ets:new(?CLIENT_TABLE, [named_table, public, set, {keypos, #trap_client_info.id},
    {write_concurrency, true}, {read_concurrency, true}]),
  %% Start esockd
  esockd:start(),
  Options = [{acceptors, 8},
    {max_connections, 100000},
    {tcp_options,
      [binary,
        {reuseaddr, true},
        {backlog, 512},
        {packet, 0},
        {nodelay, false}]}],
  MFA = {?MODULE, start_link, []},

  esockd:open(trap_server_connector, Port, Options, MFA).


start_link(Transport, Socket) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Socket]])}.
init([Transport, Socket]) ->
  case Transport:wait(Socket) of
    {ok, NewSocket} ->
      {ok, {IP, Port}} = Transport:peername(NewSocket),
      io:format("New socket connected: Ip is :~p and port is ~p ~n", [IP, Port]),
      Transport:setopts(Socket, [{active, once}]),
      InitState = #trap_connection_info{transport = Transport, socket = NewSocket, ip = IP},
      %% 先挂起来等认证,防止恶意连接
      erlang:send_after(?EXPIRE_TIME, self(), wait_for_auth),
      %% 进入下一次循环
      gen_server:enter_loop(?MODULE, [], InitState);
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% ping包
handle_cast(ping, #trap_connection_info{socket = Socket, transport = Transport} = State) ->
  io:format("pong ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<"pong">>),

  {noreply, State};
%% protocol_error
handle_cast(protocol_error, #trap_connection_info{socket = Socket, transport = Transport} = State) ->
  io:format("protocol_error ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<"protocol_error">>),
  gen_tcp:close(Socket),
  {stop, normal, State};

%% connect
handle_cast(connect, #trap_connection_info{socket = Socket, transport = Transport} = State) ->
  io:format("connect ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<"connect_ok">>),

  {noreply, State};

%% auth 认证客户端,改变状态即可
handle_cast({auth, {Mode, PayLoad}}, #trap_connection_info{ip = IP, socket = Socket} = State) ->
  <<ClientIdLength:16, ClientId:ClientIdLength/binary>> = PayLoad,
  case (ClientId) of
    %% 测试用的，后面会加入数据库支持
    <<"client0012">> ->
      io:format("Auth success and clientid is:~p ~n", [ClientId]),
      ID = case ets:last(?CLIENT_TABLE) of
             '$end_of_table' ->
               1;
             I ->
               I + 1
           end,
      ets:insert(?CLIENT_TABLE, {ID, #trap_client_info{id = ID, client_id = ClientId, ip = IP, socket = Socket, auth = true, mode = Mode}}),
      gen_server:cast(self(), auth_ok),
      {noreply, State#trap_connection_info{id = ID, auth = true, client_id = ClientId, mode = Mode}};
    _Other ->
      io:format("Auth failure and clientid is:~p ~n", [ClientId]),
      gen_server:cast(self(), auth_failure),
      {noreply, State}
  end;

%% auth_ok
handle_cast(auth_ok, #trap_connection_info{socket = Socket, transport = Transport} = State) ->
  send_to_client(Transport, Socket, <<"auth_ok">>),
  {noreply, State};
%% send
handle_cast({send, {Mode, PayLoad}}, State) ->
  %% <<DataLength:16, ClientIdLength:8, ClientId:ClientIdLength/binary, Data/binary>>
  <<DataLength:16, ClientIdLength:8, ClientId:ClientIdLength/binary, Data:DataLength/binary>> = PayLoad,
  %% io:format("PayLoad is ~p~n", [PayLoad]),
  case Mode of
    1 -> io:format("TCP Mode ClientId is ~p Data is ~p~n", [ClientId, Data]);
    _ -> io:format("Trap Mode ClientId is ~p Data is ~p~n", [ClientId, Data])
  end,

  {noreply, State};

%% auth_failure
handle_cast(auth_failure, #trap_connection_info{socket = Socket, transport = Transport} = State) ->
  io:format("Auth failure and Socket is:~p ~n", [Socket]),
  send_to_client(Transport, Socket, <<"auth_failure">>),
  gen_tcp:close(Socket),
  {stop, normal, State};

%% packet_arrived
handle_cast({packet_arrived, _RemoteSocket, BinData}, State) ->
  DataLength = byte_size(BinData),
  case DataLength > 1 of
    false ->
      gen_server:cast(self(), protocol_error),
      {noreply, State};
    true ->
      try
        <<Mode:2, Type:6, PayLoad/binary>> = BinData,
        io:format("Raw BinData is ~p~n", [BinData]),
        case Mode of
          1 ->
            case Type of
              0 ->
                gen_server:cast(self(), ping);
              1 ->
                gen_server:cast(self(), connect);
              4 ->
                gen_server:cast(self(), {auth, {Mode, PayLoad}});
              7 ->
                gen_server:cast(self(), {send, {Mode, PayLoad}});
              O ->
                io:format("Other type: ~p ~n", [O])
            end,
            {noreply, State#trap_connection_info{mode = Mode}};
          0 ->
            {noreply, State#trap_connection_info{mode = Mode}};
          _ ->
            {noreply, State}
        end
      catch
        _Reason:_E ->
          io:format("Error :~p Reason : ~p ~n", [_E, _Reason]),
          gen_server:cast(self(), protocol_error),
          {noreply, State}
      end
  end;

handle_cast({error, Socket, Message}, #trap_connection_info{socket = Socket, transport = Transport} = State) ->
  io:format("error ~p~n", [Socket]),
  Transport:send(Socket, Message),
  Transport:setopts(Socket, [{active, once}]),
  {stop, normal, State}.


%%
%% 消息接收处理
%%

handle_info({tcp, RemoteSocket, BinData}, State) ->
  gen_server:cast(self(), {packet_arrived, RemoteSocket, BinData}),
  {noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
  io:format("handle_info tcp_error ~p , Error from: ~p~n", [Reason, Socket]),
  ets:match_delete(?CLIENT_TABLE, {'_', #trap_client_info{socket = Socket, _ = '_'}}),
  {stop, normal, State};

handle_info({tcp_closed, Socket}, State) ->
  io:format("Socket cloesd: ~p ~n", [Socket]),
  ets:match_delete(?CLIENT_TABLE, {'_', #trap_client_info{socket = Socket, _ = '_'}}),
  {stop, normal, State};


%%
%% 这个是定时检查认证状态,防止恶意连接
%%
handle_info(wait_for_auth, #trap_connection_info{auth = Auth} = State) ->
  case Auth of
    true ->
      {noreply, State};
    false ->
      gen_server:cast(self(), auth_failure),
      {noreply, State};
    _ ->
      {stop, normal, State}
  end;

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% send to client
send_to_client(Transport, Socket, Data) ->
  Transport:send(Socket, Data),
  Transport:setopts(Socket, [{active, once}]).

```

下面是监督者:

```erlang
-module(trap_tcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).
%% 服务端启动
start_link(Port) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).


init([Port]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {trap_tcp_server, {trap_tcp_server, start, [Port]},
    Restart, Shutdown, Type, [trap_tcp_server]},

  {ok, {SupFlags, [AChild]}}.

```

启动app运行:

```erlang

-module(trap_app).

-behaviour(application).

-export([start/2, stop/1]).
-define(TCP_PORT, 5000).

start(_StartType, _StartArgs) ->
  io:format("Trap Tcp Server Started at Port:~p~n", [?TCP_PORT]),
  trap_tcp_server_sup:start_link(?TCP_PORT).

stop(_State) ->
  ok.

```

我们把服务端运行在5000端口.

## 6.客户端实现

下面的代码就是连接和登陆,send协议的实现,协议实现请看之前的<<TCP协议设计>>这篇文.

```erlang

-module(trap_client).
-export([start/1, start/0]).

start(Port) ->
  case gen_tcp:connect("127.0.0.1", Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
    {ok, ServerSocket} ->
      logger:info("Request login"),
      auth(ServerSocket, <<"client0012">>),
      ControlPid = spawn(fun() -> loop(ServerSocket) end),
      gen_tcp:controlling_process(ServerSocket, ControlPid),
      ControlPid;
    {error, Why} ->
      logger:error("Error ~p", [Why])

  end.
start() ->
  start(5000).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      case Data of
        <<"auth_ok">> ->
          logger:info("Auth Ok."),
          send_packet(Socket, <<"client0012">>, <<"HeloWoodGoodJobAhahahahaha">>);
        <<"auth_failure">> ->
          logger:info("Auth Failure.");
        <<"send_ok">> ->
          logger:info("Receive data ~p", [Data])
      end,
      loop(Socket);
    {error, closed} ->
      logger:error("Socket [~p] close", [Socket])

  end.

%% auth
auth(Socket, ClientId) when is_binary(ClientId) ->
  %% Tcp
  Mode = 1,
  %% Auth
  Type = 4,
  %% Length
  DataLength = byte_size(ClientId),
  gen_tcp:send(Socket, <<Mode:2, Type:6, DataLength:16, ClientId/binary>>).

%% send
send_packet(Socket, ClientId, Data) when is_binary(Data) ->
  %% Tcp
  Mode = 1,
  %% Auth
  Type = 7,
  %% Length
  ClientIdLength = byte_size(ClientId),
  DataLength = byte_size(Data),
  gen_tcp:send(Socket, <<Mode:2, Type:6, DataLength:16, ClientIdLength:8, ClientId:ClientIdLength/binary, Data:DataLength/binary>>).

```

## 7.总结

本文主要讲了esockd框架的基础知识和一个最简单协议服务端实现实战.