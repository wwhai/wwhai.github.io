---
title: 'Mosquitto 二次开发 (二)：Mosquitto Broker插件机制'
date:  2023-03-25 08:32:01
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

## Mosquitto 二次开发 (二)：Mosquitto Broker插件机制
Mosquitto提供了扩展机制，它原生支持用C语言来实现一些自己的插件，然后动态加载进Mosquitto运行时，极大的丰富了Mosquitto的功能，可以让我门在不改变Mosquitto本身的情况下实现自己的扩展功能。

### Eclipse Mosquitto Broker 插件机制概述
Mosquitto的插件本质上是个C语言的动态链接库（或者说就是个兼容POSIX规范的链接库），如果你熟悉C语言可以用 C语言来实现插件，同样你也可以用其他语言比如C++或者Pascal【[https://macpgmr.github.io/MacXPlatform/PascalDynLibs.html](https://macpgmr.github.io/MacXPlatform/PascalDynLibs.html)】之类的编程语言。实现他的特定函数即可，Mosquitto的插件本身不是太难。

值得一提的是，因为Mosquitto支持两个版本的插件，导致目前互联网上很多教程很杂乱，因为很多教程写的比较早，绝大部分讲的是 V4 版本的插件开发，但是在Mosquitto 2.0时代，V4插件因为设计上比较复杂已经不建议使用了，截止目前为止（2023年3月25日）Mosquitto的插件版本推荐使用 V5。下面是 `mosquitto_plugin.h`里面的原话：
```txt
This interface is available from v2.0 onwards.
```
因此本文主要研究 V5 版本的插件，后面提到的所有“插件”默认都是V5版本。关于V4版本插件知识，可通过网络检索，不过作者认为已经没有研究的意义。

### Eclipse Mosquitto Broker 插件生命周期
Mosquitto插件只有2个生命周期回调：
1. mosquitto_plugin_init
    mosquitto_plugin_init 回调是插件加载的时候调用的，其函数定义如下：
    ```c
    int mosquitto_plugin_init(mosquitto_plugin_id_t *identifier,
                              void **userdata,
                              struct mosquitto_opt *options,
                              int option_count)
    ```
    - `identifier` 是插件的ID，Mosquitto内部用的，其代表了当前插件的一个句柄，用来注册回调使用
    - `user_data` 原本貌似是代表用户数据，但是这个设计有问题，因为用户数据一般是用户在业务代码里面传入的，而不是Mosquitto传入，因此这里一直是NULL，而且在Mosquitto的源码里面也是直接给了个NULL值，因此这个参数可能是Mosquitto团队留下的扩展功能
    - `options` 外部配置信息，也就是 mosquitto.conf 里面的那些K-V对，但是这个比较坑的一点是，他会把所有的配置传进来，因此你需要遍历这个指针数组
    - `opt_count` 外部配置信息的数目，配合 options 遍历使用

2. mosquitto_plugin_cleanup
    mosquitto_plugin_cleanup 回调是插件卸载的时候调用的，其函数定义如下：
    ```c
     int mosquitto_plugin_cleanup(void *userdata,
                                struct mosquitto_opt *options,
                                int option_count)
    ```
    - `userdata` 此处的 userdata 就是你在插件里面传入的真实值了 和 mosquitto_plugin_init 不是一个含义
    - `options` 外部配置信息，也就是 mosquitto.conf 里面的那些K-V对，但是这个比较坑的一点是，他会把所有的配置传进来，因此你需要遍历这个指针数组
    - `opt_count` 外部配置信息的数目，配合 options 遍历使用

除了生命周期回调，还有一个用来兼容V4版本插件的一个回调：mosquitto_plugin_version，因为Mosquitto有混乱的插件体系，需要一个回调确定一个版本，因此这个函数在V5插件里面是个固定写法。
```c
int mosquitto_plugin_version(int supported_version_count, const int *supported_versions)
```
其两个参数没有用，因此无需关注。在实际插件中，只需要简单粗暴直接返回`5`即可：
```c
int mosquitto_plugin_version(int supported_version_count, const int *supported_versions)
{
    return 5;
}
```
### Eclipse Mosquitto Broker 插件开发实战
经过上面的分析，实际上我们已经掌握了插件的一个框架，下面我们来实战一下，首先新建一个文件，名称为: mosquitto_plugin_demo.c
```c
// 加载插件
int mosquitto_plugin_init(mosquitto_plugin_id_t *identifier,
                        void **userdata,
                        struct mosquitto_opt *options,
                        int option_count)
{
    printf("mosquitto_plugin_init\n");
    return 0;
}
// 卸载插件
int mosquitto_plugin_cleanup(void *userdata,
                                struct mosquitto_opt *options,
                                int option_count)
{
    printf("mosquitto_plugin_cleanup\n");
    return 0;
}
// 版本
int mosquitto_plugin_version(int supported_version_count,
                             const int *supported_versions)
{
    printf("mosquitto_plugin_version\n");
    return 5;
}
```
上述示例就是一个最简单的插件，通过编译以后，当启动mosquitto的时候会在每个周期输出对应的字符串。下面给一个编译Makefile：
```makefile
include ../../config.mk

.PHONY : all binary check clean reallyclean test install uninstall

PLUGIN_NAME=mosquitto_plugin_demo
DEPS=-lmosquitto
all : binary

binary : ${PLUGIN_NAME}.so

${PLUGIN_NAME}.so : ${PLUGIN_NAME}.c
	$(CROSS_COMPILE)$(CC) $(PLUGIN_CPPFLAGS) $(PLUGIN_CFLAGS) $(PLUGIN_LDFLAGS) -fPIC -shared $< -o $@ ${DEPS}

reallyclean : clean
clean:
	-rm -f *.o ${PLUGIN_NAME}.so *.gcda *.gcno

check: test
test:

install: ${PLUGIN_NAME}.so
	# Don't install, these are examples only.
	#$(INSTALL) -d "${DESTDIR}$(libdir)"
	#$(INSTALL) ${STRIP_OPTS} ${PLUGIN_NAME}.so "${DESTDIR}${libdir}/${PLUGIN_NAME}.so"

uninstall :
	-rm -f "${DESTDIR}${libdir}/${PLUGIN_NAME}.so"

```
在根目录下执行:
```
make
```
如果没有错误，就会在目录下生成一个 mosquitto_plugin_demo.so 的动态库，这个就是最终的插件，将其加入到Mosquitto配置文件中, 我们新建一个 test.conf 文件：
```conf
listener 1883
allow_anonymous true
plugin  ../plugins/mosquitto_plugin_demo/mosquitto_plugin_demo.so
```
然后启动Mosquitto：`mosquitto -c test.conf`，就可以运行上面的插件。

## 总结
我们展示了一个最基本的插件结构，实际上所有的插件都是经过上述模板来扩展功能的，可将其保存为一个代码模板，每次新开发插件的时候直接先生成一个模板，然后逐渐扩展即可。