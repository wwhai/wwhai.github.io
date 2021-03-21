---
title: 基于Erlang纯手工实现一个WEB框架
date:  2021-03-21 06:49:29
index_img: /static/22.jpg
tags: 
    - 杂文
categories: 
    - 杂文
author: wangwenhai
---
本文作者：[wangwenhai] # 概要：基于Erlang纯手工实现一个WEB框架
<!-- more -->


# 基于Erlang纯手工实现一个WEB框架

## 1.前言

记得很久以前，我还是个java程序员的时候，使用过很多Java世界的WEB框架，例如国产的JFinal，还有风靡全球的SpringMVC系列，甚至我开始入门的时候还学过上古时代（我是2015年正式自学Java WEB，Struts是2008年左右的框架）的Struts框架（这个框架后来被爆出各种漏洞，逐步被人淡忘），当时觉得很神奇，框架这个东西为何能被这么多人认可和使用？设计框架的人是怎么想的？一连串的问题在我脑海里环绕了好几年，直到我大学毕业后工作，逐步明白了SpringMVC的工作原理以后才逐步知道了框架是怎么回事，用一句话来说，框架就是把很多人做的公共重复工作给抽象出来，公开给其他人用，减少一些机械劳动的一套软件。当时还尝试写出来个简单的框，得益于Java的动态特性，基于Java的反射来实现一个简单的WEB框架很简单，于是在2019年我才真正自己实现了个简单的JavaWEB框架。

后来这几年，因为工作逐步磨灭了我的一些想法和精力，再没有精力去尝试实现一些有趣的东西，“写个框架”这个想法就被搁置了很久。尽管后来的工作逐渐接触了很多其他编程领域的WEB框架，比如Python的Flask，Django等等，但是始终没有自己去研究过一些细节。

契机在2020年，我到了杭州现在公司以后。2020年我转行做了Erlang（此处有记录《我为何转向Erlang》），从此告别了我熟悉很多年的Java世界。

做了Erlang以后发现很多问题，Erlang是小众领域的编程语言，很多库，依赖都不完善，就连个好点的WEB开发框架都没有，像SpringMVC这种重量级生产框架，Erlang世界是空白。当时我就在想，为何目前没有人愿意给Erlang造轮子呢？但是随着Erlang的深入学习和工作中的积累我发现，Erlang并不适合做SpringMVC那种级别的WEB项目，它的精髓在并发和容错。

直到有一天我看到了我们公司的大牛同事写的一个很有趣的框架：minirest，我突然觉得Erlang来实现一些WEB框架其实还是比较简单的，受了minirest的启发以后，我突然想起多年前那个想法：自己写个WEB框架试试。

于是2020年下半年那段时间，晚上下班以后回到家里就研究Erlang的一些深度技术，还有参考minirest的设计和实现原理，重新打造设计了一套WEB框架：FWEB。FWEB的含义是：Functional WEB或者是 Funny WEB，有两层含义，一是“函数式”，另一是“有点意思”。

后来因为各种原因，FWEB还是没有实现完全，但是其设计思路和架构设计极具学习价值，于是经过长时间的整理和总结，我把当时的一些想法整理出来，以供后人学习。

## 2.基础架构



说起WEB框架的架构，我们首先来看下Java的SpringMVC的基础架构：

![image-20210321154811292](/uploads/Implement-Web-framework-with-erlang/static/image-20210321154811292.png)

基本原理就是用户请求发送到前端控制器，然后前端控制器解析参数，分别来调度到不同的Mapping路由映射中去，调用对应的方法，最后把结果反馈给用户，如果我们觉得这个图很复杂，其实SpringMVC可以简化一下，简化成一个***WEB框架的基本架构***。

![image-20210321155754294](/uploads/Implement-Web-framework-with-erlang/static/image-20210321155754294.png)

其实我们简化到这里就很明显了，一个基础位WEB框架需要的东西，其实就这3部分：

1. 前端调度器
2. 动作执行器
3. 数据加工器

前端调度器处理路由，然后处理好的参数交给动作执行器去执行某个函数，然后把执行的返回值，加工成数据返回给前端。

### 2.1架构设计

现在我们来设计我们的FWEB的基础架构：

![image-20210321161052440](/uploads/Implement-Web-framework-with-erlang/static/image-20210321161052440.png)

从上图可知，FWEB包含了这几个关键组件：

1. Dispatcher
2. Interceptor chain
3. Actions function

接下来我们一一解释这些组件的功能。

### 2.3基础组件

- Dispatcher

  这是核心前端调度器，主要用来解析URL参数，例如我们访问如下URL：

  ```sh
  http://host:port/users?name=fweb
  ```

  此时这个请求到FWEB的前端调度器以后，会做以下几件事：

  1. 提取请求参数：users?name=fweb;
  2. 查找路由Action Module;
  3. 查找Module是否包含Action：users
  4.  如果有`users`则把 name=fweb 参数传递给function。

  整个过程其实很容易理解，就是一个解析***URL参数***的过程。路由的格式为：

  ```sh
  http://host:port/mapping@action?k=v&m=n………
  ```

- Interceptor chain

  如果你熟悉Java WEB 就会很熟悉一个组件叫：拦截器。我们这里的拦截器也是同样的功能。只要有一个拦截器不通过，则整个拦截链被中断。

- Actions function

  Actions指的就是最终的动作，映射到Erlang里面就是函数，例如下面这个Actions的功能就是返回一个字符串“hello”：

  ```erlang
  index() ->
      "hello".
  ```

接下来我们看一下应用。

## 3.基本应用

假设我们现在需要实现一个简单的功能：返回一个页面：

- 先增加处理器

  ```erlang
  Handlers = [#{name => article_handler, mapping => "/index"}],
  fweb:add_mapping(Handlers).
  ```

- 开始写Actions

  ```erlang
  -module(index).
  -export([index/2]).
  index(_, _) ->
    {html, fweb:render("/index.html")}.
  ```

- 页面内容

  ```html
  <!DOCTYPE html>
  <html lang="zh-CN">
  <head>
      <meta charset="UTF-8">
      <title>Welcome Fweb World</title>
      <link rel="stylesheet" href="">
  </head>
  
  <body>
      <h1>Hello('_')World,This is fweb</h1>
  </body>
  
  </html>
  ```

- 访问URL

  ```
  http://localhost:9990/index@index
  ```
  ![image-20210321164820592](/uploads/Implement-Web-framework-with-erlang/static/image-20210321164820592.png)

## 4.总结

我们简要讲了一个简单的WEB框架的设计过程，通过这个框架，让我们更能认识到Erlang的一些开发技巧和特性。

## 5.参考资料

- FWEB：[wwhai/fweb: A lightweight web framework Implement by Erlang/OTP (github.com)](https://github.com/wwhai/fweb)
- MINIREST：[emqx/minirest: A Mini RESTful API Framework (github.com)](https://github.com/emqx/minirest)