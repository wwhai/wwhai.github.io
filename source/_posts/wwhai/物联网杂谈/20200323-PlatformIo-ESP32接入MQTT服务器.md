---
title: PlatformIo-ESP32接入MQTT服务器
date:  2020-03-23 16:16:22
index_img: /static/2.jpg
tags: 
- C++
- PlatformIo

categories: 
- 物联网高级开发技术

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文主要讲一个简单案例：通过PlatformIO来开发ESP32开发板，使其可以通过MQTT协议连接服务器发送数据。
<!-- more -->

# 1.环境准备

之前的文章有讲怎么安装PlatformIO，如果没有环境，先驱翻一下前面的文章。本文主要直接讲一个简单的实践。

# 2.新建项目

## 1.首先添加依赖

在`platformio.ini`中添加如下内容：

```ini
[env:esp32dev]
monitor_speed = 115200
platform = espressif32
board = esp32dev
framework = arduino
lib_deps =  AsyncMqttClient
```

## 2.构建项目

```
pio run
```

到这里要是没问题的话，可以一路顺利进行下去。

## 3.源码解释

接下来直接讲一下源代码：

```c++
/*
This example uses FreeRTOS softwaretimers as there is no built-in Ticker library
*/
extern "C"
{
#include "freertos/FreeRTOS.h"
#include "freertos/timers.h"
}
#include <AsyncMqttClient.h>
#include <EEPROM.h>
#include <WiFi.h>
#include <WiFiType.h>
#include <esp_event.h>
#include <string.h>
// WIFI SSID
#define WIFI_SSID "206-public"
// WIFI 密码
#define WIFI_PASSWORD "665544332211"
// MQTT Server
#define MQTT_HOST IPAddress(192, 168, 2, 142)
// MQTT 端口
#define MQTT_PORT 1883
// 下行
const char *c2sTopic = "687giyhbU^&YYHOU/c2s";
// 上行
const char *s2cTopic = "687giyhbU^&YYHOU/s2c";
// 状态
const char *stateTopic = "687giyhbU^&YYHOU/state";

AsyncMqttClient asyncMqttClient;
TimerHandle_t mqttReconnectTimer;
TimerHandle_t wifiReconnectTimer;

void connectToWifi()
{
  Serial.println("Connecting to Wi-Fi...");
  WiFi.begin(WIFI_SSID, WIFI_PASSWORD);
}

void connectToMqtt()
{
  Serial.println("Connecting to mqtt server...");
  asyncMqttClient.setClientId("687giyhbU^&YYHOU");
  asyncMqttClient.setCredentials("Y&TOHUBo8yhiol&UJ)", "&UNBkjL63541tgHB");
  asyncMqttClient.connect();
}

void WiFiEvent(WiFiEvent_t event)
{
  // Serial.printf("[WiFi-event] event: %d\n", event);
  switch (event)
  {
  case SYSTEM_EVENT_STA_GOT_IP:
    Serial.println("WiFi connected");
    Serial.print("IP address: ");
    Serial.println(WiFi.localIP());
    connectToMqtt();
    break;
  case SYSTEM_EVENT_STA_DISCONNECTED:
    Serial.println("WiFi lost connection");
    // 当WIFI链接失败的时候,需要停止MQTT的连接定时器
    xTimerStop(mqttReconnectTimer, 0);
    xTimerStart(wifiReconnectTimer, 0);
    break;
  default:
    break;
  }
}
/**
 * 
 * MQtt连接成功以后,订阅s2c,QOS=2
 * 
 */
void onMqttConnect(bool sessionPresent)
{
  Serial.print("Connected to MQTT;Session present:");
  Serial.println(sessionPresent);
  asyncMqttClient.subscribe(s2cTopic, 2);
  // asyncMqttClient.publish("test/lol", 0, true, "test 1");
}

void onMqttDisconnect(AsyncMqttClientDisconnectReason reason)
{
  Serial.println("Disconnected from MQTT server.");

  if (WiFi.isConnected())
  {
    xTimerStart(mqttReconnectTimer, 0);
  }
}

void onMqttMessage(char *topic, char *payload, AsyncMqttClientMessageProperties properties, size_t len, size_t index, size_t total)
{
  Serial.print("Data received:");
  Serial.println(payload);
}

void onMqttPublish(uint16_t packetId)
{
  Serial.println("Publish successfully.");
}

void setup()
{
  Serial.begin(115200);
  Serial.println("EZlinker sdk V0.1");
  mqttReconnectTimer = xTimerCreate("mqttTimer", pdMS_TO_TICKS(2000), pdFALSE, (void *)0, reinterpret_cast<TimerCallbackFunction_t>(connectToMqtt));
  wifiReconnectTimer = xTimerCreate("wifiTimer", pdMS_TO_TICKS(2000), pdFALSE, (void *)0, reinterpret_cast<TimerCallbackFunction_t>(connectToWifi));

  WiFi.onEvent(WiFiEvent);

  asyncMqttClient.onConnect(onMqttConnect);
  asyncMqttClient.onDisconnect(onMqttDisconnect);
  asyncMqttClient.onMessage(onMqttMessage);
  asyncMqttClient.onPublish(onMqttPublish);
  asyncMqttClient.setServer(MQTT_HOST, MQTT_PORT);

  connectToWifi();
}

void loop()
{
}
```



其中比较重要的是：

```c
// WIFI SSID
#define WIFI_SSID "206-public"
// WIFI 密码
#define WIFI_PASSWORD "665544332211"
// MQTT Server
#define MQTT_HOST IPAddress(192, 168, 2, 142)
// MQTT 端口
#define MQTT_PORT 1883
```

上述代码就是用来配置MQTT服务器的，包含了WIFI的SSID，密码，服务器的IP，端口等等。

而数据接收相关代码在这个函数中实现：

```c
void onMqttMessage(char *topic, char *payload, AsyncMqttClientMessageProperties properties, size_t len, size_t index, size_t total)
{
  Serial.print("Data received:");
  Serial.println(payload);
}
```

你可以在这里解析相关的数据格式，比如JSON，XML。

如果需要上报数据，可以用这个函数：

```c++
asyncMqttClient.publish("上报的Topic", 【Qos：0,1,2】, true, "数据内容");
```

## 4.服务器搭建

前期测试阶段建议使用EMQ作为本地测试节点。

## 5.总结

本文给出了一个简单的Demo，通过ESP32开发板来实现和MQTT服务器互联，大家可以尝试一下，逐步实现状态监控，比如用LED实现网络状态监控，或者用OLED实现监控。还有一些比较复杂的操作，比如自动配网，保存配置到flash中，希望大家可以多多交流。