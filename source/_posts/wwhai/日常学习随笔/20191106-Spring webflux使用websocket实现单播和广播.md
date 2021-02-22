---
title: Spring webflux使用websocket实现单播和广播
date:  2019-11-06 11:12:13
index_img: /static/22.jpg
tags: 
- java
- Springboot
- websocket

categories: 
- Springboot

author: wangwenhai
---
本文作者：[gerry] # 概要：先前搜了一些关于reactive websocket的示例都是单个session收到消息后又发送给自己，例如：session.send(session.receive())，这种形式和单次http的作用无差别，
<!-- more -->

﻿@[TOC](Spring webflux使用websocket实现单播和广播)

- 写这个配置类的原因：

先前搜了一些关于reactive websocket的示例都是单个session收到消息后又发送给自己，例如：session.send(session.receive())，这种形式和单次http的作用无差别，并没有体现双向通讯。Spring boot又还未支持像1.*版本对stomp协议的支持，直接使用SimpMessagingTemplate就能点对点单播和广播。然后在GitHub上找到一个靠谱的例子，而且真的是只有一个，资料太少，参考后先实现了进行单播和广播，再加上对响应式编程渐渐理解，最终形成这个配置代码。

----------

使用 **Spring boot 2.1.2.release**，该版本的Spring boot还未对reactive websocket进行自动装配，需要开发人员手动配置，代码地址供参考：
https://github.com/Gerry-Pan/Live/blob/master/src/main/java/cn/com/pan/live/config/WebSocketConfiguration.java

 - 注册HandlerMapping和WebSocketHandlerAdapter

Spring boot启动时会扫码BeanFactory中的org.springframework.web.reactive.HandlerMapping类型的Bean，给websocket增加映射操作类：

    @Bean
    public HandlerMapping handlerMapping(WebSocketHandler webSocketHandler) {

   		Map<String, WebSocketHandler> map = new HashMap<String, WebSocketHandler>();
   		map.put(webSocketHandlerPath, webSocketHandler);

   		SimpleUrlHandlerMapping mapping = new SimpleUrlHandlerMapping();
   		mapping.setUrlMap(map);
   		mapping.setOrder(10);

   		return mapping;
    }
    

    @Bean
    public WebSocketHandlerAdapter webSocketHandlerAdapter() {
    	return new WebSocketHandlerAdapter();
    }

 - 注册Websocket握手时的处理类WebSocketHandler

该方法中定义了接收和发送消息：

    @Bean
    public WebSocketHandler webSocketHandler() {

   		UnicastProcessor<String> messageProcessor = this.messageProcessor();
   		Flux<String> messages = messageProcessor.replay(0).autoConnect();
   		Flux<String> outputMessages = Flux.from(messages);

   		return (session) -> {
   			session.receive().map(WebSocketMessage::getPayloadAsText).subscribe(messageProcessor::onNext,
   					messageProcessor::onError, messageProcessor::onComplete);

   			return session.getHandshakeInfo().getPrincipal().flatMap((p) -> {
   				session.getAttributes().put("username", p.getName());
   				return session.send(outputMessages.filter((payload) -> this.filterUser(session, payload))
   						.map((payload) -> this.generateMessage(session, payload)));
   			}).switchIfEmpty(Mono.defer(() -> {
   				return Mono.error(new BadCredentialsException("Bad Credentials."));
   			})).then();
   		};
   	}
UnicastProcessor订阅了org.springframework.web.reactive.socket.WebSocketSession.receive()的数据流，session接收消息后将消息传给UnicastProcessor，而session.send订阅UnicastProcessor，当UnicastProcessor中有数据进入，会触发session.send。而且同理用在[**reactor kafka**](https://github.com/Gerry-Pan/Live/blob/master/src/main/java/cn/com/pan/live/config/KafkaConfiguration.java)上。除了UnicastProcessor，还有几个Processor，比如EmitterProcessor，TopicProcessor，ReplyProcessor，DirectProcessor等等等等，感兴趣的童鞋就去研究一下。

 - 根据消息过滤用户

有多少用户开启websocket连接进来就存在多少session实例，session.send就会被触发多少次，但是消息只发给对应的用户，所以需要筛选，如何筛选是可以自定义，比如根据payload中的字段指定的用户名筛选。filterUser方法返回true则是表示该消息是发送给此session，会继续调用generateMessage方法，false则不是，generateMessage方法不被调用，例如：

    protected boolean filterUser(WebSocketSession session, String payload) {
    	System.out.println(session.getAttributes());
    	System.out.println("filterUser-----" + Thread.currentThread().getName());
    	return true;
    }

**当然这种方式不够科学，本人会再研究如何实现对应的session.send被触发，而不是所有的被触发再进行过滤。**