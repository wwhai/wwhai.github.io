---
title: ErlangTCP服务器实践
date:  2019-10-15 16:55:12
index_img: /uploads/1571135439867.png
tags: 
- erlang
- otp
- tcp

categories: 
- erlang

author: wangwenhai
---
本文着重讲解ErlangTCP服务器从简单到复杂的集中构建形式，深入理解Erlang的TCP机制。
<!-- more -->

## Erlang TCP基础知识

###### Erlang对TCP支持非常完善，有很多同步和异步库使用。Erlang提供了一个精巧的模块：gen_tcp，来完成基础的Socket通信操作.详情见文档：http://erlang.org/doc/man/gen_tcp.html



## ErlangTCP的模式

**主动模式{active, true}**，非阻塞方式接收消息，但在系统无法应对超大流量请求时，客户端发送的数据过快，而且超过服务器可以处理的速度，那么，系统就可能会造成消息缓冲区被塞满，出现持续繁忙的流量的极端情况，系统因请求过多而溢出，造成Erlang虚拟机内存不足而崩溃。

**被动模式{active, false}**，阻塞方式接收消息，底层的TCP缓冲区可用于抑制请求，并拒绝客户端的消息，在接收数据的地方都会调用gen_tcp:recv，造成阻塞（单进程模式下就只能消极等待某一个具体的客户端Socket ，很危险）。需要注意的是，操作系统可能还会做一些缓存允许客户端机器继续发送少量数据，然后才将其阻塞，但这个时候Erlang还没有调用recv函数。

**混合型模式（半阻塞，{active, once}）**，主动套接字仅针对一条消息，在控制进程发送完一个消息数据后，必须显式地调用inet:setopts(Socket, [{active, once}]) 重新激活以便接受下一个消息（在此之前，系统处于阻塞状态）。可见，混合型模式综合了主动模式和被动模式的两者优势，可实现流量控制，防止服务器被过多消息淹没。

Joe老爷子在书中建议: 混合型模式是最合适的。

## 一个最简单的TCP服务



```erlang
server() ->
    {ok, LSock} = gen_tcp:listen(8080, [binary, {packet, 0}, {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = do_recv(Sock, []),
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(LSock),
    Bin.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.
```

代码解读：

```erlang
    {ok, LSock} = gen_tcp:listen(8080, [binary, {packet, 0},  {active, false}]),
```
第2行`gen_tcp:listen`函数会在本地打开一个端口进行监听，这里我们打开8080；后面的列表是具体的选项：`[binary, {packet, 0},  {active, false}]`,binary表示是二进制类型的数据，packet表示数据包头，这里0字节，后期会详细讲。{active, false}表示是一个被动套接字。

```erlang
{ok, Sock} = gen_tcp:accept(LSock),
```

第三行代码表示开始监听这个socket，也就是我们本地的8080端口，类似于我们开了一个tomcat一样，可以对外提供服务。

```erlang
do_recv(Sock, Bs) ->
case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
        do_recv(Sock, [Bs, B]);
    {error, closed} ->
        {ok, list_to_binary(Bs)}
end.
```
第四行是一个自定义函数，我们通过`gen_tcp:recv(Sock, 0)` 来接受Socket的数据，如果成功，则返回{ok, B},元组，其中B是数据(binary),其中12行很重要，这里涉及到了Erlang的尾递归，如果在其他语言，比如Java、C里面，无限递归会消耗栈，导致内存泄漏，但是erlang的尾递归是经过优化的，就和正常循环一样去使用即可。

13行则是出错以后的处理，直接返回一个信息即可。

上面是最基础的一个TCP服务模式，我们运行以后就可以在本地起一个服务器，接下来我们看一个最简单的客户端。



## TCP客户端

客户端相对来讲比较简单：

```erlang
client() ->
    {ok, Sock} = gen_tcp:connect("localhost", 8080,  [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).
```

通过 `gen_tcp:connect`连接到我们刚打开的服务端，然后发送数据，到服务端那边会直接打印出来。如果在Windows下，可以通过werl来测试:

CMD运行：``werl`,然后输入ErlangTCP客户端代码：

![1571135439867](/uploads/1571135439867.png)

就可以直接可视化进行调试了。

