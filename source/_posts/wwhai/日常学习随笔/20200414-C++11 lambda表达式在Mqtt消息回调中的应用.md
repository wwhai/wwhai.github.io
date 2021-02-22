---
title: C++11 lambda表达式在Mqtt消息回调中的应用
date:  2020-04-14 16:41:38
index_img: /static/19.jpg
tags: 
- C++

categories: 
- C++11

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文通过一个简单的Demo来实现C++11环境下的Mqtt消息回调函数
<!-- more -->
# 1.本文概述
近期在用Qt5-C++开发Mqtt客户端的时候，消息回调我用了普通的函数指针的形式，后来发现更骚的是Lambda匿名函数的写法比较精炼，在这里记录学习一下过程。
# 2.原始需求
mqtt消息到达以后，一开始用了一个函数指针来处理：
```c++
void handleMsg(char msg[], int qos, char topic[], void (*onMessage)(char[], int, char[])) {
	onMessage(msg,  qos,  topic);
}
```
当消息到达的时候我们只需要实现这个onMessage函数就行：

```c++
void  onMessage (char msg[], int qos, char topic[]) {
	cout << "onMessage:" << "MSG:" << msg << " QOS:" << qos << " Topic:" << topic << endl;
};
```
调用：
```c++
int main()
{
	char topic[] = "test/";
	char msg[] = "json";
	int qos = 1;
	handleMsg(msg, qos, topic, onMessage);
	return 0;
}
```

到这里即可实现消息回调。上面是经典的函数指针的形式来实现的。
# 3.改造函数
我们换个更精炼的方法来实现，使用C++11的Lambda表达式，改造一下onMessage;
```c++
int main()
{
	char topic[] = "test/";
	char msg[] = "json";
	int qos = 1;
	handleMsg(msg, qos, topic, [](char msg[], int qos, char topic[]) {
		cout << "onMessage:" << "MSG:" << msg << " QOS:" << qos << " Topic:" << topic << endl;
	});
	return 0;
}

```
直接传入匿名内部函数就可以实现，这样看起来更加精炼，看起来像是“函数定义和调用同时发生”。拆解一下看，这段代码：
```c++
[](char msg[], int qos, char topic[]) {
		cout << "onMessage:" << "MSG:" << msg << " QOS:" << qos << " Topic:" << topic << endl;
}
```
其实是一个简化写法：
```c++
auto onMessage[](char msg[], int qos, char topic[]) {
		cout << "onMessage:" << "MSG:" << msg << " QOS:" << qos << " Topic:" << topic << endl;
}
```
而我们的回调原型：
```c++
void handleMsg(char msg[], int qos, char topic[], void (*onMessage)(char[], int, char[])) {
	onMessage(msg,  qos,  topic);
}
```
最后一个参数其实就是函数。利用了C++11的Lambda函数特性，可以省去名称。