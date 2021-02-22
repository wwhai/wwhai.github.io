---
title: ThingsBoard源码分析-JavaMqtt实现原理
date:  2020-01-02 14:26:06
index_img: /uploads/4185175-54ccd179f77b8390.webp
tags: 
- java

categories: 
- 源码分析

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文主要讲一下著名物联网应用ThingsBoard中Java实现Mqtt协议的一些细节.
<!-- more -->

## 1.概述

ThingsBoard是一个github上star数非常高的项目,主要特色是开源,纯Java技术栈,对于学习者来说降低了门槛.本人闲暇之余,学习了一下其设计思路和源码,大致把自己的心得和一些研究成果做一个记录.

这里主要记录了ThingsBoard的基本架构和部分源码分析(主要是Java实现Mqtt部分),并未研究如何使用,如果享要使用,请移步此处查看文档:https://thingsboard.io.

## 2.Mqtt协议简介

首先提一下MQTT 协议吧,MQTT 全称为 Message Queuing Telemetry Transport（消息队列遥测传输）是一种基于**发布/订阅**范式的“轻量级”消息协议，由 IBM 发布。目前广泛应用于物联网.相关知识请移步这里:http://mqtt.p2hp.com

其中ThingsBoard支持的基本协议就是MQTT 协议.

## 3.ThingsBoard简介

ThingsBoard是一个开源平台，用于收集和可视化物联网设备的数据。可以将来自任意数量设备的数据发送到云服务器，在云服务器中可以通过可自定义的仪表板查看或共享。

Thingsboard支持提供设备连接的标准协议是MQTT，CoAP和HTTP，并支持云和本地部署。 它提供了30多个可自定义的小部件，允许您为大多数物联网用例构建最终用户自定义仪表板。thingsboard.io开源的物联网平台主要特点：实时数据可视化和远程设备控制自定义仪表板的30个可定制小部件可定制的规则，插件，小部件和传输实现允许监视客户端和提供服务器端设备属性。支持多租户安装即装即用。支持MQTT和HTTP协议的传输加密。失败的节点可以在没有停机的情况下进行更换.Thingsboard分为专业版和社区版，社区版是开源的，专业版是收费的.

## 4.Netty简介

因为ThingsBoard的Mqtt协议处理相关部分是用Java实现的,其中使用的框架是Netty,所以这里我们需要着重介绍一下Netty.

Netty是Java的一个NIO框架,架构图如下所示(取自官网):

![img](/uploads/4185175-54ccd179f77b8390.webp)

Netty 是一个利用 Java 的高级网络的能力，隐藏其背后的复杂性而提供一个易于使用的 API 的客户端/服务器框架。
Netty 是一个广泛使用的 Java 网络编程框架（Netty 在 2011 年获得了Duke's Choice Award，见https://www.java.net/dukeschoice/2011）。它活跃和成长于用户社区，像大型公司 Facebook 和 Instagram 以及流行 开源项目如 Infinispan, HornetQ, Vert.x, Apache Cassandra 和 Elasticsearch 等，都利用其强大的对于网络抽象的核心代码。

> 摘自[《Essential Netty In Action》

说白了就是Netty是一个性能非常优秀的网络通讯框架,可以自定义各种协议,开发基于TCP/IP栈的应用协议.换句话来说,Netty的核心功能就是让你自己实现协议,因此属于比较高级的Java内容,要求大家的通信基础知识扎实.

关于更多的这里就不介绍了,详情请移步:https://netty.io/index.html.



## 7.Netty实现Mqtt协议

ThingsBoard的Mqtt协议处理是用Netty实现的,我们来看看基本的代码结构.

首先我们看下Mqtt协议实现的核心类:https://github.com/thingsboard/thingsboard/tree/master/netty-mqtt/src/main/java/org/thingsboard/mqtt.

![image-20200102145611920](/uploads/image-20200102145611920.png)

包含的是Mqtt协议的具体实现和协议处理相关的类,我们挑其中一个比较重要的协议报文处理类:https://github.com/thingsboard/thingsboard/blob/master/netty-mqtt/src/main/java/org/thingsboard/mqtt/MqttChannelHandler.java,来分析一下代码.

1. ### 类继承关系

   ```java
   final class MqttChannelHandler extends SimpleChannelInboundHandler<MqttMessage> {
   //.......
   }
   ```

   ​        Mqtt协议处理器继承的是SimpleChannelInboundHandler,这个类主要就是处理消息和转换消息,吧网络字节流转换成MqttMessage类,其中:SimpleChannelInboundHandler<T> 是一个Netty提供给用户的接口,用来自动转换网络字节流.

   

2. ### 协议报文处理

   下面我们摘取一段代码来进行简单分析:

   ```java
   //......
       protected void channelRead0(ChannelHandlerContext ctx, MqttMessage msg) throws Exception {
           //处理Mqtt报文,类型见下表
           switch (msg.fixedHeader().messageType()) {
               case CONNACK:
                   handleConack(ctx.channel(), (MqttConnAckMessage) msg);
                   break;
               case SUBACK:
                   handleSubAck((MqttSubAckMessage) msg);
                   break;
               case PUBLISH:
                   handlePublish(ctx.channel(), (MqttPublishMessage) msg);
                   break;
               case UNSUBACK:
                   handleUnsuback((MqttUnsubAckMessage) msg);
                   break;
               case PUBACK:
                   handlePuback((MqttPubAckMessage) msg);
                   break;
               case PUBREC:
                   handlePubrec(ctx.channel(), msg);
                   break;
               case PUBREL:
                   handlePubrel(ctx.channel(), msg);
                   break;
               case PUBCOMP:
                   handlePubcomp(msg);
                   break;
           }
       }
   //.....
   ```

   

   > ​        也许你对channelRead0这个命名比较迷惑,其实这个问题作者本人也说了,是不小心随手写的:"silly mistake".关于这个类的重写,我个人总结出来的一种写法比较好,可以作为参考:
   >
   > ```java
   > import io.netty.channel.ChannelHandlerContext;
   > import io.netty.channel.ChannelInboundHandlerAdapter;
   > import io.netty.util.ReferenceCountUtil;
   > import io.netty.util.internal.TypeParameterMatcher;
   > 
   > /**
   >  * 重写了一下Netty提供的SimpleChannelInboundHandler
   >  * @param <I>I表示一个抽象 ，特指实现了Message的具体消息的类
   >  */
   > public abstract class MessageReceiveHandler<I> extends ChannelInboundHandlerAdapter {
   >     /**
   >      * TypeParameterMatcher 是为了判断泛型的类, 然后根据传入的泛型来确定类型拦截器
   >      * 比如泛型的类是Mqtt消息，然后识别出来以后就会使用Mqtt的拦截器Handler
   >      */
   >     private final TypeParameterMatcher matcher;
   >     private final boolean autoRelease;
   > 
   >     protected MessageReceiveHandler() {
   >         this(true);
   >     }
   > 
   >     private MessageReceiveHandler(boolean autoRelease) {
   >         this.matcher = TypeParameterMatcher.find(this, MessageReceiveHandler.class, "I");
   >         this.autoRelease = autoRelease;
   >     }
   > 
   >     protected MessageReceiveHandler(Class<? extends I> inboundMessageType) {
   >         this(inboundMessageType, true);
   >     }
   > 
   >     private MessageReceiveHandler(Class<? extends I> inboundMessageType, boolean autoRelease) {
   >         this.matcher = TypeParameterMatcher.get(inboundMessageType);
   >         this.autoRelease = autoRelease;
   >     }
   > 
   >     /**
   >      * 这个方法的作用是：判断是否是泛型规定的消息类型
   >      *
   >      * @param msg
   >      * @return
   >      * @throws Exception
   >      */
   >     private boolean acceptInboundMessage(Object msg) throws Exception {
   >         return this.matcher.match(msg);
   >     }
   > 
   >     /**
   >      * 从消息管道中读取消息，然后处理成泛型识别出来的类型(理解起来很蛋疼)
   >      *
   >      * @param ctx
   >      * @param msg
   >      * @throws Exception
   >      */
   > 
   >     public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
   >         boolean release = true;
   > 
   >         try {
   >             if (this.acceptInboundMessage(msg)) {
   >                 this.messageReceived(ctx, (I) msg);
   >             } else {
   >                 release = false;
   >                 ctx.fireChannelRead(msg);
   >             }
   >         } finally {
   >             if (this.autoRelease && release) {
   >                 ReferenceCountUtil.release(msg);
   >             }
   > 
   >         }
   > 
   > 
   >     }
   > 
   >     /**
   >      * 在上一步的 channelRead 中，已经处理了消息转换，所以我们实现这个方法的时候，I 其实就是我们具体的消息了
   >      *
   >      * @param channelHandlerContext 具体的处理器的上下文
   >      * @param i                     具体消息
   >      * @throws Exception
   >      */
   >     protected abstract void messageReceived(ChannelHandlerContext channelHandlerContext, I i) throws Exception;
   > 
   > 
   >     /**
   >      * 断线
   >      * @param ctx
   >      * @throws Exception
   >      */
   >     @Override
   >     public void channelInactive(ChannelHandlerContext ctx) throws Exception {
   >         super.channelInactive(ctx);
   >     }
   > }
   > ```
   >
   > 

   ​        channelRead0方法负责处理流入的消息,其中ChannelHandlerContext表示当前连接进来的客户端的上下文,通俗点讲就是Socket的封装,而MqttMessage则是泛型T传进来的类型,表示这里已经是Mqtt报文.

   ​        在此之前本人也尝试有用Netty实现一个MqttServer,但是工作量比较大,做的不完善,经过参考别人的代码,最终修改出来了一个自定义的处理器可做参考:https://github.com/wwhai/EasyJMqttServer/blob/master/mqtt-broker/src/main/java/com/easyiot/iot/mqtt/server/core/BrokerHandler.java.

   ​        Mqtt报文有很多种,详细见下表,含义请看这里:http://mqtt.p2hp.com/mqtt311.
   
   | 报文类型    | 值   | 描述                     |
   | ----------- | ---- | ------------------------ |
   | CONNECT     | 1    | 客户端向代理发起连接请求 |
   | CONNACK     | 2    | 连接确认                 |
   | PUBLISH     | 3    | 发布消息                 |
   | PUBACK      | 4    | 发布确认                 |
   | PUBREC      | 5    | 发布收到（QoS2）         |
   | PUBREL      | 6    | 发布释放（QoS2）         |
   | PUBCOMP     | 7    | 发布完成（QoS2）         |
   | SUBSCRIBE   | 8    | 客户端向代理发起订阅请求 |
   | SUBACK      | 9    | 订阅确认                 |
   | UNSUBSCRIBE | 10   | 取消订阅                 |
   | UNSUBACK    | 11   | 取消订阅确认             |
   | PINGREQ     | 12   | PING请求                 |
| PINGRESP    | 13   | PING响应                 |
   | DISCONNECT  | 14   | 断开连接                 |

   switch语句就是为了处理不同的协议包.我们拿其中的CONACK包来分析一下:
   
   ```java
    private void handleConack(Channel channel, MqttConnAckMessage message) {
      switch (message.variableHeader().connectReturnCode()) {
      //当连接成功以后,返回应答报文,然后回应客户端
       case CONNECTION_ACCEPTED:
           this.connectFuture.setSuccess(new MqttConnectResult(true, MqttConnectReturnCode.CONNECTION_ACCEPTED, channel.closeFuture()));               this.client.getPendingSubscriptions().entrySet().stream().filter((e) -> !e.getValue().isSent()).forEach((e) -> {
                 channel.write(e.getValue().getSubscribeMessage());
                 e.getValue().setSent(true);
   });
       this.client.getPendingPublishes().forEach((id, publish) -> {
       if (publish.isSent()) return;
          channel.write(publish.getMessage());
          publish.setSent(true);
          if (publish.getQos() == MqttQoS.AT_MOST_ONCE) {
          publish.getFuture().setSuccess(null); //We don't get an ACK for QOS 0
          this.client.getPendingPublishes().remove(publish.getMessageId());
        }
   });
         channel.flush();
         if (this.client.isReconnect()) {
         this.client.onSuccessfulReconnect();
         }
         break;
          //此处处理的就是连接失败的情况,对应状态码看下面给出的表
          case CONNECTION_REFUSED_BAD_USER_NAME_OR_PASSWORD:
          case CONNECTION_REFUSED_IDENTIFIER_REJECTED:
          case CONNECTION_REFUSED_NOT_AUTHORIZED:
          case CONNECTION_REFUSED_SERVER_UNAVAILABLE:
          case CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION:
          this.connectFuture.setSuccess(new MqttConnectResult(false, message.variableHeader().connectReturnCode(), channel.closeFuture()));
          channel.close();
      // Don't start reconnect logic here
      // 翻译:不要在这里进行重连.这是为什么?因为客户端都断开了,这个状态明确知道,  所以不用再次尝试连接
           break;
      }
}
   ```

   ​        CONNACK是代理用来响应客户端CONNECT的报文。代理向客户端发送的第一个报文必须是CONNACT。CONNACK有一个固定报头，一个可变报头，但是不带有荷载。CONNACK报文的可变报头为定长2字节。第一字节的0位表示是否有会话存在。如果代理上已经有请求连接的客户端的会话，且连接请求的清除会话标识为0，则该位为1，否则该位为0。客户端可以根据这一位的值采取响应行为，比如（重新）订阅主题等。

   ​        CONNACK报文的可变报头的第二字节为返回码。如果CONNECT请求的格式正确，但是代理依然不能允许客户端连接，则返回码为一个非零值。如果连接成功，则返回0。
   
   | 值    | 返回码含义                                            |
   | ----- | ----------------------------------------------------- |
   | 0     | 成功，连接请求被接受。                                |
   | 1     | 拒绝连接，不可接受的协议版本。                        |
   | 2     | 拒绝连接，不被允许的身份识别符（Client Identifier）。 |
   | 3     | 拒绝连接，服务器不可用。                              |
   | 4     | 拒绝连接，无效的用户名和密码。                        |
| 5     | 拒绝连接，客户端无授权。                              |
   | 6-255 | 系统保留。                                            |

   ​        我们从代码实现可知,Netty其实就是做了协议和报文封装,具体的实现和分析,转换,还是根据具体自定义的规范去实施,类似于上述Mqtt的CONNACK报文处理一样.MqttConnectResult是一个给客户端的回应结果封装.

   ​        按照这个思路,我们继续来看下publish报文的处理,分别处理了三种QOS的报文:
   
   ```java
    private void handlePublish(Channel channel, MqttPublishMessage message) {
   //根据不同的QOS来进行处理   
    switch (message.fixedHeader().qosLevel()) {
   //QOS=1的情况            
   case AT_MOST_ONCE:
   invokeHandlersForIncomingPublish(message);
   break;
   //QOS=1的情况
   case AT_LEAST_ONCE:
   invokeHandlersForIncomingPublish(message);
   if (message.variableHeader().packetId() != -1) {
   MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBACK, false, MqttQoS.AT_MOST_ONCE, false, 0);
   MqttMessageIdVariableHeader variableHeader = MqttMessageIdVariableHeader.from(message.variableHeader().packetId());
                       channel.writeAndFlush(new MqttPubAckMessage(fixedHeader, variableHeader));
   }
   break;
   //QOS=2的情况
   case EXACTLY_ONCE:
   if (message.variableHeader().packetId() != -1) {
   // MqttFixedHeader:Mqtt固定报文头的封装格式
   MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBREC, false, MqttQoS.AT_MOST_ONCE, false, 0);
   MqttMessageIdVariableHeader variableHeader = MqttMessageIdVariableHeader.from(message.variableHeader().packetId());
   MqttMessage pubrecMessage = new MqttMessage(fixedHeader, variableHeader);
   
   MqttIncomingQos2Publish incomingQos2Publish = new MqttIncomingQos2Publish(message, pubrecMessage);
                      this.client.getQos2PendingIncomingPublishes().put(message.variableHeader().packetId(), incomingQos2Publish);
   message.payload().retain();
   incomingQos2Publish.startPubrecRetransmitTimer(this.client.getEventLoop().next(), this.client::sendAndFlushPacket);
   channel.writeAndFlush(pubrecMessage);
   }
      break;
     }
   }
   
   /**
   *处理入站报文
   **/
      private void invokeHandlersForIncomingPublish(MqttPublishMessage message) {
           boolean handlerInvoked = false;
          //遍历所有的订阅,然后交给handler去处理
           for (MqttSubscription subscription : ImmutableSet.copyOf(this.client.getSubscriptions().values())) {
               if (subscription.matches(message.variableHeader().topicName())) {
                   if (subscription.isOnce() && subscription.isCalled()) {
                       continue;
                   }
                   message.payload().markReaderIndex();
                   subscription.setCalled(true);
                   subscription.getHandler().onMessage(message.variableHeader().topicName(), message.payload());
                   if (subscription.isOnce()) {
                       this.client.off(subscription.getTopic(), subscription.getHandler());
                   }
                   message.payload().resetReaderIndex();
                   handlerInvoked = true;
               }
           }
           if (!handlerInvoked && client.getDefaultHandler() != null) {
               client.getDefaultHandler().onMessage(message.variableHeader().topicName(), message.payload());
           }
           message.payload().release();
}
   ```

   ​        我们不难看出,其实就是按照MQTT协议的实现文档进行了Java Class封装和处理.技术难点在于吃透协议规范.

   ​        还有其他的协议报文大同小异,基本就都是精细活,按照官方文档,挨个实现即可.当我们掌握了Netty的这种用法以后,可以很容易实现各种协议,比如可以实现一个Redis客户端,或者是Mysql客户端等等,实现起来都是比较容易的.
   
   ​        Netty框架是十分强大的,如果精通以后可以开发出高性能,高并发的应用,很适合用在物联网或者通信相关的场景.用Java开发的MqttServer效率也是非常高,建议大家深入研究一下.

## 8.总结

​        本文主要讲了Netty实现Mqtt协议的关键部分:协议报文处理.