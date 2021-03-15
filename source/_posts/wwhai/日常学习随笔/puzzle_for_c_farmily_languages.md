---
marp: true
---
<!-- _backgroundColor: #99CCFF -->

# C语言家族的一些历史遗留和困惑
![bg left:25%](https://picsum.photos/720?image=29)

```C
#include <stdio.h>
int main(){
    printf("hello world\n");
    return 0;
}
```

---
# 字符串表示
```C
const char * s1 = "hello world";
char s2[] = "hello world";
```
## 困惑点
- 两种语义化类型表示同一目标，产生歧义；
- 提出“字符串指针”，打破了“指针是一个内存编号”的规范；

---
# 字符串改良建议
1. const char * 类型建议直接产生新类型叫：原子，或者tag；
2. 取消”字符串“的概念；直接叫字符序列；或者字符列表
3. 统一字符序列的指针是一个int值的概念，取消”静态字符串常量指针“的概念

---
# 示例代码
```C
// 定义一个静态字面值常量称之为原子
atom a = "ok";
// 字符序列指针可以取原子的地址
char *cptr = &a;
```
从上面的优化方案可以看出，“字符串”用”原子“类型直接替代以后，消除了困惑和歧义

---
# 数组的困惑
```shell
array的谷歌翻译:
    动词
        排列
        array, arrange, range, put in order
        部署
        deploy, dispose, arrange, array
    名词
        排列
        arrangement, array, permutation
        矩阵
        matrix, array
        序列
        sequence, array, suite
        大批
        bulk, large quantity, array, legion, considerable
```
上述翻译，就没有一个含义和“数”相关，这是中文翻译的败笔；个人认为数组的翻译堪比”套接字“,"句柄","鲁棒",完全是译者水平不足的体现。

---
# 数组改良建议
中文名称改为：序列，或者列表比较合适，比较适合直观理解。
## 新的概念
> 序列：一系列相同元组的集合。
示例代码：
```C
int a[4] = {0,1,2,3};

T a[n] = {...};
```

---
# 类型定义
typedef:冗长而且没有语义化，建议将该关键字改良成`type`.
```C
type int int_t;
type char char_t (*fp)(void);
```

---
# 函数指针
C的函数指针和指针混杂起来，不利于语义化和理解
```C
void (*fun)(void *);
```
(*fun)本质上是个指针，可以用一个关键字类型替代

----
## 改进建议
新增*fun*类型直接定义函数
```C
// define
void call(fun f){
    f();
}
// fun with name
void fun f = (void *){}
call(f);
// anynomous fun
call(void fun(void *){}){f()}
```
---
## 说明
虽然C可以通过宏定义来实现各种类型，但是我一直认为原生支持是最有利于理解和学习的.
```C
typedef void (*fun)(void*);
```

```C
void fun f = (void *){}
```
# 开箱即用不好吗？

---
# Java的困惑
## 基本类型
```
数据类型        默认值
byte            0
short           0
int             0
long            0L
float           0.0f
double          0.0d
char            'u0000'
boolean         false
```
基本类型是历史遗留，是当年JamsGalslin这些人设计的不足之处，从谷歌相关资料也可证实该说法。其实基本类型不应该出现在任何OOP编程语言中，基本类型出现在OOP语言中打破了规则，而且引入的新概念会含糊不清，

---
## 抽象类
抽象类和接口的功能几乎是重合的，完全可以用抽象类来当做接口使用，所以Interface或者abstract class意义何在？该处也是遗留问题，也许最完美的设计应该是没有abstract class。这点从后生的编程语言可以看出，比如JS，Python等，就没有混杂的这两种类型。
> 纯正的OOP思想就是继承，代码复用。
