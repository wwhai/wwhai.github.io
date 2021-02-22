---
title: PlatformIo-基于Arduino框架开发单片机
date:  2020-03-23 16:17:05
index_img: /static/20.jpg
tags: 
- C++
- PlatformIo

categories: 
- 物联网高级开发技术

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文主要讲一下使用PlatformIo，基于Arduino框架开发单片机项目。
<!-- more -->

# 1.背景介绍

​        目前玩Arduino的朋友，用得最多的也许是Arduino官方的ArduinoIDE，说实话这个IDE真不好用，甚至不如一个记事本。有时候启动速度简直不能忍受，还有包管理做的也不好。依赖都不清楚。这种情况下可能大家很想要一个友好的IDE来开发Arduino应用。刚好，PlatformIo配合Vscode可以做。

# 2.关于PlatformIo

​        PlatformIO 是开源的物联网开发生态系统。提供跨平台的代码构建器、集成开发环境（IDE），兼容 Arduino，ESP8266等。甚至说是一个框架，子啊这个框架之下我们除了可以开发Arduino，还可以开发Arm甚至是WindowsAPP。

# 4.PlatformIo安装

## 1.通过VScode安装

直接通过插件搜索【PlatformIo】即可：

![platformio-ide-vscode-pkg-installer.png](/uploads/platformio-ide-vscode-pkg-installer.png)

## 2.命令行安装

因为PlatformIo是Python实现的一个库，所以可以直接用Python的Pip包管理器安装：

```python
Py2：pip install -U platformio
Py3：pip3 install -U platformio
```

## 3.更多安装方式

​        除此之外还支持其他IDE或者编辑器安装，请参考这里：https://docs.platformio.org/en/latest/core/installation.html



# 3.Arduino初试

​        我们试着来新建一个Arduino项目。这里我们假设项目名字就叫:HelloWorld.首先在硬盘上新建一个目录：HelloWorld.

```shell
mkdir HelloWorld
cd HelloWorld
```

​         然后初始化项目：

```shell
 platformio project init --board uno
```

​        表示我们新建了一个项目，使用了uno开发板作为基础的硬件。

​        如何添加依赖？首先找到`platformio.ini`这个文件，其实这个文件类似Java项目中常见依赖工具的Mavel的pom.xml,或者是node项目中的package.json,都是用来统一管理依赖的。依赖文件如下所示：

```ini
[env:uno]
platform = atmelavr
framework = arduino
board = uno
```

​        表示我们使用的是atmelavr这个平台的硬件库，用了Arduino的框架来对uno开发板进行开发。然后我们在src目录下新建一个main.cpp的入口文件，下面的代码想必大家很熟悉了，这里就不做赘述。

```C++

#include "Arduino.h"

#ifndef LED_BUILTIN
#define LED_BUILTIN 13
#endif

void setup()
{
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop()
{
  digitalWrite(LED_BUILTIN, HIGH);

  delay(1000);

  digitalWrite(LED_BUILTIN, LOW);

  delay(1000);
}
```

​        到此为止，我们的项目基本结构就完成了，目录结构如下：

```txt
HelloWorld
├── lib
│   └── README
├── platformio.ini
└── src
    └── main.cpp
```

​        如果你做过WEB开发，就很清楚node和maven这两个依赖管理的处理步骤，项目新建好了以后就开始构建。

- `platformio run` 下载依赖
- `platformio run --target upload`：编译上传
- `platformio run --target clean`：清理编译结果
- `platformio run -e uno`：编译指定的Profile，比如我们常见的dev，prod，test等等
- `platformio run -e uno -t upload`：编译指定的Profile，然后上传。

![/platformio-demo-wiring.gif](/uploads/platformio-demo-wiring.gif)

​        在此有一个已经写好的ESP通过Mqtt传输数据的Demo项目，供给大家学习参考：https://github.com/wwhai/ezlinker_arduino_sdk

# 4.总结

​        本文主要介绍了一种全新的开发Arduino的方法，希望能有助于大家提高开发效率。