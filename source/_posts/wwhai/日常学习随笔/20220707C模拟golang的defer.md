---
title: 用C模拟golang的defer
date:  2022-06-07 11:47:22
index_img: /static/12.jpg
tags:
  - C语言片段
categories:
  - CS基础知识
author: wangwenhai
---
本文作者：[wangwenhai] # 概要：用C模拟golang的defer->
 <!-- more -->
# 用C模拟golang的defer
## 简介
众所周知，Golang有个defer语法，用来在***计算之后, 返回之前***干一些事情，很适合我们释放资源和善后处理等，比如下面这个文件操作：
```go
func main(){
    f,err := os.Open("demo.log")
    if err != nil {
        panic(err)
    }
    defer f.Close()
}

```
这让我们写代码的时候非常方便。但是C没有这个特性怎么办呢？其实可以用一定的手段来实现。
## 实现
## Sejmp
C 库宏 int setjmp(jmp_buf environment) ：创建本地的jmp_buf缓冲区并且初始化，用于将来跳转回此处。这个子程序保存程序的调用环境于env参数所指的缓冲区，env将被longjmp使用。如果是从setjmp直接调用返回，setjmp返回值为0。如果是从longjmp恢复的程序调用环境返回，setjmp返回非零值。
## 案例
```c
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
typedef int process(char *err);
static jmp_buf G_ENV;
static char G_ERR[20];
//--------------------------------------------

void throw(char *err)
{
    strcpy(G_ERR, err);
    longjmp(G_ENV, 1);
}
void defer(process *p)
{
    if (setjmp(G_ENV) > 0)
    {
        p(G_ERR);
    }
}

void http_post1()
{
    throw("302: Host not found");
}
void http_post2()
{
    printf("200: ok\n");
}
int defer_func(char *err)
{
    printf("Catched Exceptions: %s\n", err);
    return 1;
}
//
// 模拟实现 defer
//
int main()
{
    defer(defer_func);
    http_post1(); // 模拟异常
    http_post2(); // 模拟正常
    return 0;
}
```
我们很巧妙的使用setjmp来实现了模拟defer。其实setjmp有更多高级玩法，有待大家去发现，这里就不多赘述了。

## 参考
- https://man7.org/linux/man-pages/man3/setjmp.3.html
- https://www.cnblogs.com/hazir/p/c_setjmp_longjmp.html