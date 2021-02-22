---
title: 通过WebSocket监控EMQX的设备上下线
date:  2020-01-08 17:15:36
index_img: /static/1.jpg
tags: 
- Springboot
- Emqx

categories: 
- Java

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文主要用WebSocket实现了一个简单的监控器,监控设备上下线.
<!-- more -->

## 1.概述

很多人想通过WEB界面来实时监控设备上下线消息,于是我做了个简单的Demo供大家参考.

本项目用Mqtt-paho来实现客户端代理的,实验之前记得装依赖.

## 2.服务端代码

```java
package com.ezlinker.app.config.emqxproxy;

import com.alibaba.fastjson.JSONObject;
import com.corundumstudio.socketio.SocketIOClient;
import com.corundumstudio.socketio.SocketIOServer;
import com.ezlinker.app.config.mqtt.MqttProxyClient;
import com.ezlinker.app.config.socketio.EchoEventMessage;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.annotation.Resource;

/**
 * @program: ezlinker
 * @description: 这里用来通过WS给前端发一些动态通知
 * @author: wangwenhai
 * @create: 2019-12-16 15:09
 **/
@Configuration
@Slf4j
public class EmqxEventListener {

    /**
     * MQTT代理
     */
    @Resource
    MqttProxyClient emqClient;

    /**
     * 状态标识:用来标识代理是否连接成功
     */
    private static boolean isConnectToEmqx = false;


    @Bean
    public SocketIOServer socketIoServer() {
        /*
         * 创建Socket，并设置监听端口
         */
        com.corundumstudio.socketio.Configuration socketIoConfig = new com.corundumstudio.socketio.Configuration();
        /**
         * 目前只允许本地WS连接
         */
        socketIoConfig.setHostname("127.0.0.1");
        /**
         * WS端口
         */
        socketIoConfig.setPort(2501);
        socketIoConfig.setUpgradeTimeout(10000);
        socketIoConfig.setPingTimeout(180000);
        socketIoConfig.setPingInterval(60000);
        // 认证
        socketIoConfig.setAuthorizationListener(data -> {
            // TODO 这里做个安全拦截器,WS必须带上颁发的随机Token才能连接
            return true;
        });
        SocketIOServer server = new SocketIOServer(socketIoConfig);
        server.startAsync();
        /**
         * WS 连接处理
         */
        /**
         * 当WS连接成功以后,开始连接EMQX
         */
        server.addConnectListener(this::connectToEmqx);
        /**
         * 离线回调
         */
        server.addDisconnectListener(this::disConnectToEmqx);

        return server;
    }


    /**
     * 回复消息
     *
     * @param message
     */
    private void echoEvent(SocketIOClient socketIoClient, EchoEventMessage message) {
        socketIoClient.sendEvent("echoEvent", JSONObject.toJSONString(message));
    }

    /**
     * 代理客户端
     *
     * @param ioClient
     * @return
     */

    private void connectToEmqx(SocketIOClient ioClient) {

        /**
         * 开始连接MQTT
         */

        try {
            /**
             * 把前一个给踢下去
             */
            if (emqClient.isConnected()) {
                emqClient.disconnect();
            } else {
                MqttConnectOptions mqttConnectOptions = new MqttConnectOptions();
                mqttConnectOptions.setConnectionTimeout(10);
                mqttConnectOptions.setCleanSession(true);
                mqttConnectOptions.setAutomaticReconnect(true);
                mqttConnectOptions.setUserName("ezlinker_event_listener");
                mqttConnectOptions.setPassword("password".toCharArray());
                emqClient.connect(mqttConnectOptions);

            }
        } catch (MqttException e) {
            e.printStackTrace();
            log.error("连接EMQX失败" + e.getMessage());
            isConnectToEmqx = false;
        }
        if (emqClient.isConnected()) {
            isConnectToEmqx = true;

            try {
                emqClient.subscribe("$SYS/brokers/+/clients/+/#", 2, (s, mqttMessage) -> {
                    System.out.println(mqttMessage.toString());
                    EchoEventMessage m0 = new EchoEventMessage();
                    m0.setCode(200);
                    m0.setDebug(true);
                    m0.setMsg(JSONObject.parseObject(mqttMessage.toString()));
                    echoEvent(ioClient, m0);

                });
            } catch (MqttException e) {
                e.printStackTrace();
            }
        } else {
            isConnectToEmqx = false;
        }
    }

    /**
     * @param ioClient
     */
    private void disConnectToEmqx(SocketIOClient ioClient) {
        if (emqClient.isConnected()) {
            try {
                emqClient.disconnect();
            } catch (MqttException e) {
                log.error("内部错误:" + e.getMessage());
            }
        }
        if (isConnectToEmqx) {
            isConnectToEmqx = false;
        }

    }
}

```

## 3.Websocket端:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>app</title>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/socket.io/2.3.0/socket.io.js"></script></head>
<body>

<button id="sendBtn" onclick="send()">发送</button>
<button id="testBtn" onclick="test()">测试</button>

<div id="content-wrap"></div>
</body>
<script>
    // const socket = io('http://39.108.214.107:12001?token=32dassw4fd3r&deviceId=1');
    const socket = io('http://localhost:2501?token=32dassw4fd3r');
    socket.on('echoEvent', data => {
        window.console.log("来自服务器的回应:", data);
    });

</script>

</html>

```

## 4.效果展示

![image-20200108171853165](/uploads/image-20200108171853165.png)

然后通过前端框架,可以动态把这些数据渲染到HTML界面上面.

## 5.总结

$SYS 主题前缀: $SYS/brokers/${node}/clients/

| 主题(Topic)              | 说明                                     |
| :----------------------- | :--------------------------------------- |
| ${clientid}/connected    | 上线事件。当某客户端上线时，会发布该消息 |
| ${clientid}/disconnected | 下线事件。当某客户端离线时，会发布该消息 |

通配符:`$SYS/brokers/+/clients/+/#`,表示监听所有的节点.

