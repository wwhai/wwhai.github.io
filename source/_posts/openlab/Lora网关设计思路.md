---
title: Lora 网关设计思路
date: 2021-10-6 00:00:00
index_img: /img/article-banner.png
tags:

  - freeopenlab

categories:

  - freeopenlab

collection: freeopenlab
type: 理论研究
team : [

    ["wwhai", "CPU虚拟指令集"],
    ["LiuZY", "浏览器渲染技术"]

]
resources: [

    ["PCB电路图","#"],
    ["设计架构图","#"]

]
abstract: 本文主要讲了Lora 网关设计思路
progress : 80

---

# EMQX LoraWAN 设计思路

## 1.简介

该功能可以通过EMQX来对Lora无线通信网络进行管理和配置，控制等等，同时可以整合功能到EMQX，从而实现Lora网络透传到多种上层协议。

## 2.原理

该插件本质是一个挂载在EMQX上的串口驱动，EMQX通过和串口通信来驱动外部挂载的设备，因此需要配合Lora相关硬件设备来使用。
## 3.设计思路

在设计之初，参考了有人的Lora模块，同时了解了有人的Lora网关的一些基本操作，受到了启发，我们可以将常见的功能集成到EMQX上来，方便一些做网关的用户使用。

设计之初主要考虑到了以下几个点：

1. 驱动形式

   鉴于大部分Lora模块都是基于串口驱动的，在对比了好几种硬件之间通信协议以后，发现串口最简单，最经济实用，而且支持的平台众多，很适合做基础驱动，所以选择了串口作为驱动通信协议；

2. 管理网络

   Lora网络需要管理，比如网络状态，节点信息查看等等，所以需要设计一个精简的管理系统来集成到EMQX里面；

3. 授权鉴权

   节点的入网，发送消息，都需要鉴权，此处可以整合到EMQX的ACL机制里面。

## 4.注意事项

1. 该功能需要配合相关硬件来使用，目前只支持串口；
2. 该功能只能在Linux上运行，不支持Windows。

## 5.参考资料

```shell
RT028DS_R3000-LG产品规格书_v.1.1.1.pdf
LPWAN-review.pdf                             RT028UG_R3000_LG用户手册_v.1.0.7.pdf
Low-cost-LoRa-Collar.pdf                     USR-LG210-L_AT_V1.0.0.pdf
Low-cost-LoRa-GW-leaflet.pdf                 WAZIUP-Deployment-guidelines.pdf
Low-cost-LoRa-GW-outdoor.pdf                 WAZIUP_IoT-dev-guide.pdf
Low-cost-LoRa-GW-step-by-step.pdf            WH-L102-L-C_说明书V1.0.0.pdf
Low-cost-LoRa-GW-web-admin.pdf               WH-L102-L-P_V0.0.5.hex
Low-cost-LoRa-Ghana-iSpace-public-event.pdf  WH-L102-L-P_basic.pdf
Low-cost-LoRa-ImageIoT-step-by-step.pdf      demo-slides.pdf
Low-cost-LoRa-IoT-antennaCable.pdf           iot4all-intro-lr.pdf
Low-cost-LoRa-IoT-outdoor-step-by-step.pdf   lorawan1.0.3.pdf
Low-cost-LoRa-IoT-step-by-step.pdf           lora应用论文.pdf
Low-cost-LoRa-IoT-supported-sensors.pdf      lora终端固件升级方法.pdf
Low-cost-LoRa-IoT-using-demo-kit.pdf         low-cost-iot-hardware-parts.pdf
Low-cost-LoRa-device-leaflet.pdf             smyle-deploying-low-cost-iot.pdf
tutorial-SWHW-LoRa-WAZIUP.pdf
RESSACS16-Low-cost-LoRa-IoT-step-by-step.pdf
```




# 规范设计

## 1. 驱动规范

规定 EMQX LoraWan GateWay 对模组的驱动方式为标准串口，其中串口参数可以自己适配，默认值如下：

| 项目     | 默认值 |
| -------- | ------ |
| 波特率   | 115200 |
| 数据位   | 8      |
| 停止位   | 1      |
| 奇偶校验 | None   |

## 2. MAC层规范

### 2.1 地址规范

MAC层规定了能连接到 EMQX LoraWan Gateway 的终端一些规范：

- 信道：标准 Lora 信道，值为1-127之间

- 节点ID：用户自定义节点ID，为16位整数

- 速率：Lora射频发射速率，单位为 dBm，此速率非传输信息大小即 KB/S 的速率


设备唯一识别码 UUID 规范：
$$
UUID = CHANNEL::CID.1-CID.2-CID.3-CID.4::DR
$$

***一个案例：***100::0001-0001-0001-0001.10

规定在  EMQX LoraWan Gateway 管理的网络中，*UUID*作为唯一识别码。

> UUID 可以看做是互联网中的IP地址，而 UUID 组成中的 CHANNEL 可以看成是网关地址，CID可以看成是子网IP，DR可以看成是子网掩码。

## 2.2 计算规范

LoRa符号速率Rs可以通过以下公式计算：
$$
Rs=BW/(2^SF)
$$

LoRa数据速率DR可以通过以下公式计算：
$$
DR= SF*( BW/2^SF)*CR
$$
单位含义：

- BW: 带宽

- Rs：符号速率
- CR：编码率
- SF：扩频因子

>LoRaWAN 协议定义了一系列的数据传输速率，不同的芯片可供选择的速率范围不同，例如SX1272支持0.3-38.4kbps，SX1276支持0.018-38.4kbps的速率范围。目前能实现0.3-37.5kbps的传输速率。
>
>使用LoRa设备发送或接收的数据长度有限制，理论来说SX127x系列芯片有256 Bytes的FIFO，发射或接收256Bytes都行。但是，并不是在任何传输速率下LoRa模块的负载长度都能为256 Bytes。在传输速率较低的情况下，一次传输256 Bytes需要花费的时间极长（可能需要花费几秒甚至更长），这不利于抗干扰和交互，因此在技术处理上一般建议用户将一条长数据分割成数条小数据来进行传输

