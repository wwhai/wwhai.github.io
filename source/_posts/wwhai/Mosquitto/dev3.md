---
title: 'Mosquitto 二次开发 (三)：Mosquitto Broker部分源码解析'
date:  2023-03-25 08:32:02
index_img: /uploads/mosqdev/logo1.png
tags:
- Mosquitto二次开发

categories:
- Mosquitto二次开发

author: wwhai
---
本文作者：[wwhai] # 概要：本文主要讲解一下 Eclipse Mosquitto Broker 的插件开发技巧。
<!-- more -->

> 今天是2023年3月25日，深圳下起了瓢泼大雨，每当在这种潮湿的雨天，我的心态就开始低落起来。为了避免无限陷入这种精神折磨，挣扎着从沙发上起来，写一篇文章来转移一下注意力。


## Mosquitto 二次开发 (三)：Mosquitto Broker部分源码解析
本文主要分析一下Mosquitto的导出头文件里面的关键字段。Mosquitto的导出头文件是给用户开发者使用的，因此掌握关键数据结构还是很有必要。
Mosquitto包含了针对客户端开发的头文件`mosquitto.h`和针对服务端开发`mosquitto_broker.h`的头文件，前者一般用来开发本地客户端，后者用来开发服务器插件。我们常用的客户端源代码大多来自`mosquitto.h`。该源码可移植性强，甚至你能在嵌入式设备看到这个头文件,与之有同样功能的库是paho-mqtt:[https://github.com/eclipse/paho.mqtt.c](https://github.com/eclipse/paho.mqtt.c)，可能后者用的更多点。在本文中不做全面解析，仅仅对用户角度而言常用的有些结构做个简要介绍。
### mosquitto.h 头文件
头文件源文件可以参考此处：[https://github.com/wwhai/mosquitto/blob/master/include/mosquitto.h](https://github.com/wwhai/mosquitto/blob/master/include/mosquitto.h)。

我们首先来看看MQTT消息体结构体：
```c
struct mosquitto_message{
	int mid;
	char *topic;
	void *payload;
	int payloadlen;
	int qos;
	bool retain;
};
```
mosquitto_message 结构体是MQTT消息的结构化封装，包含了基本的MQTT属性，这些对于熟悉MQTT消息的人来说应该很熟悉，值得特别关注的是，`mid` 字段实际上是消息ID，这个ID在MQTT报文QOS为1和2的时候有效，0的时候无效，可以参考一下MQTT规范: [http://stanford-clark.com/MQTT/#msg-id](http://stanford-clark.com/MQTT/#msg-id)。其他的字段就是比较常用的，不做赘述。

接下来分析客户端编程接口。我们在用Mosquitto库开发客户端的时候，会涉及到创建客户端，设置认证信息，订阅发布等，下面用一个官方案例[https://github.com/wwhai/mosquitto/blob/master/examples/subscribe/basic-1.c](https://github.com/wwhai/mosquitto/blob/master/examples/subscribe/basic-1.c) 来分析一下客户端开发接口函数。
```c
#include <mosquitto.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


void on_connect(struct mosquitto *mosq, void *obj, int reason_code)
{
	int rc;

	printf("on_connect: %s\n", mosquitto_connack_string(reason_code));
	if(reason_code != 0){
		mosquitto_disconnect(mosq);
	}
	rc = mosquitto_subscribe(mosq, NULL, "example/temperature", 1);
	if(rc != MOSQ_ERR_SUCCESS){
		fprintf(stderr, "Error subscribing: %s\n", mosquitto_strerror(rc));
		mosquitto_disconnect(mosq);
	}
}

void on_subscribe(struct mosquitto *mosq, void *obj, int mid, int qos_count, const int *granted_qos)
{
	int i;
	bool have_subscription = false;
	for(i=0; i<qos_count; i++){
		printf("on_subscribe: %d:granted qos = %d\n", i, granted_qos[i]);
		if(granted_qos[i] <= 2){
			have_subscription = true;
		}
	}
	if(have_subscription == false){
		fprintf(stderr, "Error: All subscriptions rejected.\n");
		mosquitto_disconnect(mosq);
	}
}

void on_message(struct mosquitto *mosq, void *obj, const struct mosquitto_message *msg)
{
	printf("%s %d %s\n", msg->topic, msg->qos, (char *)msg->payload);
}


int main(int argc, char *argv[])
{
	struct mosquitto *mosq;
	int rc;
	mosquitto_lib_init();
	mosq = mosquitto_new(NULL, true, NULL);
	if(mosq == NULL){
		fprintf(stderr, "Error: Out of memory.\n");
		return 1;
	}
	mosquitto_connect_callback_set(mosq, on_connect);
	mosquitto_subscribe_callback_set(mosq, on_subscribe);
	mosquitto_message_callback_set(mosq, on_message);
	rc = mosquitto_connect(mosq, "test.mosquitto.org", 1883, 60);
	if(rc != MOSQ_ERR_SUCCESS){
		mosquitto_destroy(mosq);
		fprintf(stderr, "Error: %s\n", mosquitto_strerror(rc));
		return 1;
	}
	mosquitto_loop_forever(mosq, -1, 1);
	mosquitto_lib_cleanup();
	return 0;
}

```
首先就是`#include <mosquitto.h>`, 我们前面提到的客户端开发必须要包含该头文件。我们先从main开始看起，`mosquitto_lib_init()`是用来初始化Mosquitto的环境，这个是必须调用的。`mosquitto_new` 函数是用来生成一个客户端实例，需要注意一点：其返回值是一个指针。而	`mosquitto_connect_callback_set`、`mosquitto_subscribe_callback_set`、`mosquitto_message_callback_set` 三个函数就比较好识别，主要用来设置连接回调、订阅回调、消息到达回调。`mosquitto_connect` 函数用来和服务器建立连接，同时会用连接结果来回调 `mosquitto_connect_callback_set` 指定的函数，上面的案例中回调函数是`on_connect`。为了防止客户端退出，Mosquitto给出一个挂起函数 `mosquitto_loop_forever`，表示将进程挂起，防止退出。
`mosquitto_destroy` 用来释放 mosquitto 客户端实例，`mosquitto_lib_cleanup` 用来释放系统库资源，其二者虽然都是释放资源，但是作用对象不同。

其他省下的回调函数理解起来就比较简单了,订阅某个Topic的时候会回调 `on_subscribe` , 第一个参数 `struct mosquitto *mosq` 便是客户端实例指针。

{% note success %}
实际上对于上面的示例能理解，基本上就可以玩转 mosquitto客户端了，更多的细节可以参考其官方示例。
{% endnote %}


### mosquitto_plugin.h 头文件
mosquitto_plugin.h 头文件定义了插件的用户侧开发接口。在此之前我们说倒mosquitto支持两个版本的插件，但是目前我们只关注V5，因此此节只分析V5的插件接口。
一个插件有3个基本函数回调：
```c
 int mosquitto_plugin_version
 int mosquitto_plugin_init
 int mosquitto_plugin_cleanup
```
在第二节我们已经给出了其使用流程，再次不赘述，接下来我们关注回调如何注册。mosquitto 使用 `mosquitto_callback_register` 来注册回调，例如我们注册一个消息到达处理函数可以这么做：

```c

static int callback_message(int event, void *event_data, void *userdata)
{
	struct mosquitto_evt_message *ed = event_data;
}
int mosquitto_plugin_init(mosquitto_plugin_id_t *identifier, void **user_data, struct mosquitto_opt *opts, int opt_count)
{
	return mosquitto_callback_register(mosq_pid, MOSQ_EVT_MESSAGE, callback_message, NULL, NULL);
}
```

当注册成功后，mosquitto收到消息会直接调用 `callback_message` 函数，我们可以在里面做一些操作，比如打印出来、或者保存到Mysql等。
函数原型可以参考此处：[https://mosquitto.org/api/files/mosquitto_broker-h.html#mosquitto_callback_register](https://mosquitto.org/api/files/mosquitto_broker-h.html#mosquitto_callback_register);
与之对应的卸载回调用 `mosquitto_callback_unregister` 函数，函数原型可以参考此处：[https://mosquitto.org/api/files/mosquitto_broker-h.html#mosquitto_callback_register](https://mosquitto.org/api/files/mosquitto_broker-h.html#mosquitto_callback_unregister)。
### mosquitto_broker.h 头文件

上面的 mosquitto.h 是给客户端使用的，与此相反 mosquitto_plugin.h 、mosquitto_broker.h 是 mosquitto 的内部接口，提供了 mosquitto broker 的一些核心结构，**这些接口都不是给客户端使用的，而是为了开发服务端插件**。 该头文件内部定义了用户可操作的范畴，也就是说用户能进行二次开发的建议修改接口，其他的不建议去修改，比如 src 目录下的源码等。不过也有特例就是一些集群组件是要深度定制。
我们注册函数回调以后，发现其回调数据是一个void指针（如果学过go，这里可以理解为interface{}，虽然完全不是一回事），这个指针到底是什么含义？Mosquitto用了一系列结构体族来表示,其格式如下所示：
```c
struct mosquitto_evt_* {
    // 一些字段
};
```
上面的这个结构体实际上就是插件回调带进来的数据，比如你想针对客户端的publish消息做个处理，那么就需注册一个回调到 mosquitto 里面，当有消息来时，mosquitto会给你的接口传一个参数，这个参数就是 `mosquitto_evt_message` 结构体，以此类推，头文件里面那些结构体都实际上是回调数据。
mosquitto_broker.h 最核心的就是这些结构体族，剩下的都是一些辅助函数，比如申请内存，释放内存，获取客户端的一些属性等，可以通过查看其文档来熟练其操作。

{% note warning %}

在开发过程中建议多参考其源码注释和官方文档：[https://mosquitto.org/api/files/mosquitto-h.html](https://mosquitto.org/api/files/mosquitto-h.html)，其实注释已经很完善了。

{% endnote %}

## 总结
本文对插件的一些关键数据做了个引导指南，方便开发者快速上手。需要注意几个关键点：
- mosquitto.h 是专门针对客户端的接口
- mosquitto_plugin.h mosquitto_broker.h 是专门针对插件开发的接口
- mosquitto_plugin.h 是插件开发过程中用户侧的约束
- mosquitto_broker.h 是插件开发过程中Broker侧的约束
- mosquitto_evt_* 结构体族用来区分不同的回调消息体