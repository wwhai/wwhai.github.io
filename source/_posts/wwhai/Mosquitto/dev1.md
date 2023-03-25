---
title: 'Mosquitto 二次开发 (一)：认识 Eclipse Mosquitto Broker'
date:  2023-03-25 08:32:00
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

## Mosquitto 二次开发 (一)：认识 Eclipse Mosquitto Broker
### Eclipse Mosquitto Broker 下载
Eclipse Mosquitto Broker 是一个很强大的 Mqtt Broker，得益于C语言的优越性能，很适合我们用在一些资源紧张的硬件设备上面。关于Eclipse Mosquitto Broker 的更多细节，可上网搜索相关资料，在本系列文章中只介绍实践经验。

#### 二进制包
Eclipse Mosquitto Broker 官网提供了很多下载包，包含了主流平台常见架构。直接访问：[https://mosquitto.org/download](https://mosquitto.org/download)，选择自己的安装包即可。

#### 源码安装
除了预编译好的二进制包，mosquitto还提供了所有源码，可以使用源码的形式自助编译，源码访问此处可获得: [https://github.com/eclipse/mosquitto](https://github.com/eclipse/mosquitto)。下面给个简单的编译指令：

```sh
git clone https://github.com/eclipse/mosquitto.git
make
```

#### 依赖
本人用的系统是Ubuntu22.04，给出Ubuntu22.04下的依赖安装指令：
```sh
sudo apt-get install -y \
    libssl-dev \
    libcjson1 \
    libjson-c-dev \
    libmosquitto-dev
```

**注意**：可能需要安装一些别的依赖，常见的依赖问题可通过上网解决，相对其他一些大项目来说其实很简单。不过如果你是为了学习mosquitto的技术原理，建议用Linux来实践。在这里本人用的系统是Ubuntu22.04作为开发机。

### Eclipse Mosquitto Broker 配置
Mosquitto配置比较多，先辈们早早做了研究，此处就不赘述了，大家可以上网检索，下面这个博客讲得比较基础：[https://cloudbool.com/archive/mosquitto-basic-config.html](https://cloudbool.com/archive/mosquitto-basic-config.html)。
除了上面讲的比较基础的那些配置，此处补充点细节：
1. Mosquitto默认只能本地连接
如果没有经过任何配置，直接启动 Mosquitto ，你会发现在局域网里面连接不到，原因是 Mosquitto 默认只支持本地连接，需要配置一个 listener参数：
```
listener 1883
```
2. Mosquitto默认允许任意客户端连接
因为允许任意客户端连接，所以当你部署到设备上以后不安全，所以建议在部署环境下配置好匿名客户端处理：
```
allow_anonymous false
```
### Eclipse Mosquitto Broker 部署
Mosquitto 和别的应用一样，都是通过service的形式来部署的，下面给出在 systemctl 下的配置：
```ini
[Unit]
Description=Mosquitto MQTT Broker
Documentation=man:mosquitto.conf(5) man:mosquitto(8)
After=network.target
Wants=network.target

[Service]
ExecStart=/usr/sbin/mosquitto -c /etc/mosquitto/mosquitto.conf
ExecReload=/bin/kill -HUP $MAINPID
Restart=on-failure
ExecStartPre=/bin/mkdir -m 740 -p /var/log/mosquitto
ExecStartPre=/bin/chown mosquitto:mosquitto /var/log/mosquitto
ExecStartPre=/bin/mkdir -m 740 -p /run/mosquitto
ExecStartPre=/bin/chown mosquitto:mosquitto /run/mosquitto

[Install]
WantedBy=multi-user.target

```

上面的 service 脚本仅支持使用 systemctl 管理工具的系统，比如 debian、ubuntu等，其他的服务脚本大家可以自行探索。在 mosquitto源码里面提供了一些别的平台的脚本，可作为参考:[https://github.com/eclipse/mosquitto/tree/master/service](https://github.com/eclipse/mosquitto/tree/master/service)。

### 总结
通过本文一个概述，相信大家对mosquitto已经有了一个初步认识，重点在于掌握mosquitto的常见操作。后续文章我们继续探索mosquitto的一些内部机制和二次开发技巧。