---
title: 30分钟学Erlang (二)
date:  2021-01-16 21:01:01
index_img: /static/19.jpg
tags:
- Erlang
- OTP

categories:
- Erlang程序设计
---


作者：Shawn # 概要：那些已经有过至少一门编程语言基础，并且需要快速了解Erlang，掌握其基本要点，并马上投入工作中的人。
文章挺长，所以分成了几篇。但只要掌握了本文中提到的这些知识点，你就可以上手工作了。剩下的就是在实践中继续学习。
<!-- more -->

> 本文来自我的同事Shawn大佬的总结，Shawn大佬是目前国内Erlang领域内的权威开发架构师，30分钟学erlang系列文章是Shawn的一线经验，对于新入门的朋友很有帮助。再次感谢Shawn花时间整理博客，和贡献文档供社区学习。

# 30分钟学Erlang (一)

### 本文写给谁看的？
那些已经有过至少一门编程语言基础，并且需要快速了解Erlang，掌握其基本要点，并马上投入工作中的人。
文章挺长，所以分成了几篇。但只要掌握了本文中提到的这些知识点，你就可以上手工作了。剩下的就是在实践中继续学习。
`Erlang 读作 ai lan, er lan, er lang 都行，但你别单个字母读 E-R-L-A-N-G，那样我就不跟你玩了。`

### 什么时候用 Erlang？
Erlang 的设计目标非常明确，就是专门为大型的电信系统设计。
所以它的应用场景和设计需求就是电信领域里需要解决的问题。
主要是三个： **高并发**、**高容错**、**软实时**。电信系统负载非常大，需要同时服务大量用户的能力；同时不允许出错，电话频繁掉线会很快把客户赶到竞争对手那边；再者，即便某个通话再繁忙也不能影响其他通话的正常进行，到技术层面就是，不能因为某个任务很重，就把其他的任务的资源都占用了，while loop 占用 100% CPU是绝对不允许的。

Erlang 是实用主义的语言，属于函数式语言但并不是完全的函数式，使用 Actor 并发模型，并在此之上提供了一些更高级别的设计模式，还提供了各种常用的功能组件(e.g. HTTP客户端, XML编解码等)。Erlang 早已经脱离电信行业，飞奔到互联网行业了，因为这些年互联网行业所面临的问题，跟几十年前的电信系统越来越像。如今，[Erlang 正在进入物联网行业](http://www.emqtt.io)，它将为世界物联网的发展做出自己的贡献。

### Erlang 的不同之处

如果问到 Erlang (相较于其他编程语言) 的特别之处，有人会回答它的并发能力，有人会说并发模型或者分布式能力。这些人都不是真正理解 Erlang 设计目的的“正规军”，因为 Erlang 与其他语言最大的区别在于他的容错能力。Erlang 从设计之初就将容错作为最高级别的任务，它的大多数特性是都为容错服务的：

抢占式调度的虚拟机和基于每个进程的垃圾回收保证了软实时能力，进程并发模型保证了单个进程异常终止不会将异常扩散到其他进程，消息传递模型消除了共享内存导致的竞态条件，OTP 提供的程序组织结构 application 和 supervisor tree 更是 Erlang 独有的高容错架构，热更新功能则提供了开着跑车换轮子的能力。

所以当别人问到你 Erlang 最大的特点是什么，就两个字：容错。

## 开始吧
学习 Erlang 等小众语言的过程中，没有太多中文资料，所以这篇文章里，对于名词、概念类的，还是用英文原词不做翻译。以免造成以后学习的障碍。

### 安装和使用
##### 安装
*   For [Homebrew](http://brew.sh/) on OS X: brew install erlang
*   For [MacPorts](https://www.macports.org/) on OS X: port install erlang
*   For [Ubuntu](http://www.ubuntu.com/) and [Debian](https://www.debian.org/): apt-get install erlang
*   For [Fedora](https://getfedora.org/): yum install erlang
*   For [FreeBSD](https://www.freebsd.org/): pkg install erlang

##### 启动 Erlang Shell
安装完成后，在终端里敲 'erl' 进入 Erlang 的 [REPL](https://zh.wikipedia.org/wiki/%E8%AF%BB%E5%8F%96%EF%B9%A3%E6%B1%82%E5%80%BC%EF%B9%A3%E8%BE%93%E5%87%BA%E5%BE%AA%E7%8E%AF)，erlang shell:
```shell
➜  ~ erl
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.3  (abort with ^G)
1> io:format("hello world!~n").
hello world!
ok
2> 1 + 1.
2
3> q().
ok                                                                       
➜  ~
```

上面 Erlang shell 里:
- 第一行 `io:format("hello world!~n").` 向标准输出写了一行 `"hello world!"`, 并以 "~n" 换行结尾。最后显示的那个 `ok ` 是`io:format()` 函数的返回值。
- 第二行 `1 + 1.` 做了个加法运算，返回值是 `2`。
- 第三行 `q().` 是退出 erlang shell, 是 `init:stop().` 的快捷方式. 连续按两次` Ctrl - C`, 或者 `Ctrl - C` 之后选 `q`, 是一样的效果。

上面的两个例子展示了几个要点：
- Erlang 的每个语句都要用 `.` 结尾。
- Erlang 是函数式语言，所有的函数、表达式都必须有一个返回值。输出 "hello world" 会返回一个 atom 类型的 `ok`。
- `Ctrl - C` 然后 `q` or `a`, 或者 `q().` 会退出 shell，如果你运行了代码的话，你的应用程序会连带着一起关掉。所以线上系统千万不要 `Ctrl - C`。

### 注释
Erlang 里用 % 来做行注释，相当于C语言里的 //, 或者Python里的 #。 没有块注释。
```erlang
% I am a comment
test_fuc() ->
   "test".
```

### 基本类型
`摘取自 learn-you-some-erlang，并为你们这些有经验的程序员删减和加工`
##### Numbers
```shell
1> 2 + 15.
17
2> 49 * 100.
4900
3> 1892 - 1472.
420
4> 5 / 2.   %% 最常用的浮点数除法
2.5
5> 5 div 2.  %% div 是整除
2
6> 5 rem 2.  %% rem 是取余运算
1
...
%% 数字前面可以用 ‘#’ 来标注其 ‘Base’
%% 语法：Base#Value
%% 默认的 Base 是 10 
...
10> 2#101010.  %% 2 进制的 101010
42
11> 8#0677.  %% 8 进制的 0677
447
12> 16#AE.   %% 16 进制的 AE
174
```

##### 变量 
Erlang 是函数式语言(虽然也支持副作用)。这意味着 Erlang 里的变量 ‘ **Immutable**’ (不可变的).
Immutable variables 在设计上简单，减少了并发过程中处理状态改变带来的复杂性。理解这一点很重要。

Erlang 是动态类型的语言，但它也是强类型的语言。动态类型意味着你声明变量时不需要指定类型，而强类型是说，erlang 不会偷偷做类型转换:
```shell
1> 6 + "1".
** exception error: bad argument in an arithmetic expression
in operator  +/2
called as 6 + "1"
```

Erlang 里变量的命名有约定，必须首字母大写。因为首字母小写的，会被认为是 `atom` (原子) 类型。
`这一点在 elixir 里有改进`

正常的变量命名比如 Hello, Test. 而像 hello, test 这种的不是变量名，他们是 `atom` 类型，跟数字、字符串一样，是值类型：

```shell
1> Hello = "hello?".
"hello?"
2> Test = "testing words".
"testing words"
3> hello.
hello
4> V1 = hello.  %% bind atom hello to V1
hello
5> V1.
hello
```

Erlang 里没有赋值语句。`=` 在 Erlang 里是 `pattern matching` (匹配、模式匹配)，如果 `=` 左侧跟右侧的值不相等，就叫没匹配上，这时那个 erlang 进程会直接异常崩溃(不要害怕，erlang 里面崩溃挺正常的)。如果 `=` 左侧的变量还没有值，这次匹配过后，右侧的值就会 `bind` (绑定) 到那个变量上。
```shell
1> One.     %% 变量没绑定，不能使用。所以这里出错了。
* 1: variable 'One' is unbound
2> One = 1.   %% 匹配变量 One 与 1. 由于One 之前没有绑定过值，这里将 Number 1 绑定给 One
1
3> Un = Uno = One = 1.  
%% 1) 匹配 Un, Uno, One 和 1. One 的值是 1, 所以最右侧的 One = 1 匹配成功，匹配操作返回值是 1. 
%% 2) 然后继续与左边的 Uno 匹配。 Uno 之前没有绑定过值，所以将 1 绑定给 Uno，匹配操作返回值也是 1.  
%% 3) 同理 Un 也被绑定为 1. 返回值也是 1.
1
4> Two = One + One. %% Two 这时候被绑定为 2.
2
5> Two = 2.    %% 尝试匹配 2 = 2. 成功并返回 2.
2
6> Two = Two + 1.  %% 尝试匹配 2 = 3. 失败了，所以当前的 erlang shell 进程崩溃了，然后又自动给你启动了一个新的 erlang shell。
** exception error: no match of right hand side value 3
7> two = 2.  %% 尝试匹配一个 atom 和一个数字: two = 2. 匹配, 失败崩溃了。
** exception error: no match of right hand side value 2
8> _ = 14+3.  %% 下划线 _ 是个特殊保留字，表示 "ignore"，可以匹配任何值。
17
9> _.
* 1: variable '_' is unbound
10> _Ignore = 2.  %% 以下划线开头的变量跟普通的变量作用没有什么区别，只不过在代码中，以下滑线开头的变量告诉编译器，"如果这个变量后面我没用到的话，也不要警告我!"
2
11> _Ignore.
2 
12> _Ignore = 3.
** exception error: no match of right hand side value 3
```

##### Atoms
上面已经提到过了，Erlang 里面有 `atom` 类型，atom 类型使用的内存很小，所以常用来做函数的参数和返回值。参加 pattern matching 的时候，运算也非常快速。
在其他没有 atom 的语言里，你可能用过 `constant` 之类的东西，一个常量需要对应一个数字值或者其他类型的值。比如：
```cpp
const int red = 1;
const int green = 2;
const int blue = 3;
```
但多了这个映射，其实用起来不大方便，后面对应的值 1， 2，3 一般只是用来比较，具体是什么值都关系不大。所以有了 `atom` 就很方便了，我们从字面上就能看出，这个值是干嘛的:
```shell
1> red.
red
```

`atom` 类型支持的写法：
```shell
1> atom.
atom
2> atoms_rule.
atoms_rule
3> atoms_rule@erlang.
atoms_rule@erlang
4> 'Atoms can be cheated!'.  %% 包含空格等特殊字符的 atom 需要用单引号括起来
'Atoms can be cheated!'
5> atom = 'atom'.
atom
```
**需要注意的是**：在一个 erlang vm 里，可创建的 atom 的数量是有限制的(默认是 1,048,576 )，因为erlang 虚拟机创建 atom 表也是需要内存的。一旦创建了某个 atom，它就一直存在那里了，不会被垃圾回收。不要在代码里动态的做 string -> atom 的类型转换，这样最终会使你的 erlang atom 爆表。比如在你的接口逻辑处理的部分做 to atom 的转换的话，别人只需要用不一样的参数不停地调用你的接口，就可以攻击你。

##### Boolean 以及比较
`atom` 类型的 `true` 和 `false` 两个值，被用作布尔处理。

```shell
1> true and false.     %% 逻辑 并
false
2> false or true.     %% 逻辑 或
true
3> true xor false.     %% 逻辑 异或
true
4> not false.     %% 逻辑 非
true
5> not (true and true).
false
```
还有两个与 `and` 和 `or` 类似的操作：`andalso` 和 `orelse`。区别是 `and` 和 `or` 不论左边的运算结果是真还是假，都会执行右边的操作。而 `andalso` 和 `orelse`是短路的，意味着右边的运算不一定会执行。

来看一下比较：
```shell
6> 5 =:= 5.    %% =:= 是"严格相等"运算符，== "是大概相等"
true
7> 1 =:= 0.
false
8> 1 =/= 0.   %%  =/= 是"严格不等"运算符，/= "是相差很多"
true
9> 5 =:= 5.0.
false
10> 5 == 5.0.
true
11> 5 /= 5.0.
false
```
一般如果懒得纠结太多，用 =:= 和 =/= 就可以了。

```shell
12> 1 < 2.
true
13> 1 < 1.
false
14> 1 >= 1.      %% 大于等于
true
15> 1 =< 1.      %% 注意这个 "小于等于" 的写法，= 在前面。因为 => 还有其他的用处。。
true
17> 0 == false.  %% 数字和 atom 类型是不相等的
false
18> 1 < false.  
true
```
虽然不同的类型之间可以比较，也有个对应的顺序，但一般情况用不到的:
`number < atom < reference < fun < port < pid < tuple < list < bit string`

##### Tuples
`Tuple` 类型是多个不同类型的值组合成的类型。有点类似于 C 语言里的 `struct`。
语法是：`{Element1, Element2, ..., ElementN}`
```shell
1> X = 10, Y = 4.
4
2> Point = {X,Y}.  %% Point 是个 Tuple 类型，包含了两个整形的变量 X 和 Y
{10,4}
```
实践中，我们经常 在 tuple 的第一个值放一个 atom 类型，来标注这个 tuple 的含义。这种叫做 tagged tuple: 
```shell
1> Data1 = {point, 1, 2}.
{point,1,2}
2> Data2 = {rectangle, 20, 30}.
{rectangle,20,30}
```
后面的代码如果要处理 Data1 和 Data2 的话，只需要检查 tuple 的第一项，就知道这个 tuple 是个点坐标，还是个矩形:

```shell
3> case Data1 of
3>   {point, X, Y} -> "this is a point";
3>   {rectangle, Length, Width} -> "this is a rectangle"
3> end.
"this is a point"
```
上面用 `case` 做 pattern matching ，这个后面还要讲。

##### List
`List` 就是我们经常说的链表，数据结构里学的那个。但 List 类型在 Erlang 里使用极其频繁，因为用起来很方便。

`List` 可以包含各种类型的值:
```
1> [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
[1,2,3,{numbers,[4,5,6]},5.34,atom]
```
上面这个 list 包含了数字类型 1，2，3，一个 tuple，一个浮点数，一个 atom 类型。

来看看这个：
```
2> [97, 98, 99].
"abc"
```
卧槽这什么意思？！因为 Erlang 的 String 类型其实就是 List！所以 erlang shell 自动给你显示出来了。
就是说如果你这么写 `"abc"`, 跟 `[97, 98, 99]` 是等效的。
链表存储空间还是比纯字符串数组大的，拼接等操作也费时，所以一般如果你想用 '真 · 字符串' 的时候，用 Erlang 的 `Binary` 类型，这样写：`<<"abc">>`。这样内存消耗就小很多了。Binary 这是后话了，这篇文章里不介绍。

我知道一开始你可能不大明白 tuple 跟 list 的区别，这样吧：
- 当你知道你的数据结构有多少项的时候，用 `Tuple`；
- 当你需要动态长度的数据结构时，用 `List`。

**List 处理:**
``` shell
5> [1,2,3] ++ [4,5].       %% ++ 运算符是往左边的那个 List 尾部追加右边的 List。
%% 这样挺耗时的。链表嘛你知道的，往链表尾部追加，需要先遍历这个链表，找到链表的尾部。
%% 所以 "abc" ++ "de" 这种的操作的复杂度，取决于前面 "abc" 的长度。
[1,2,3,4,5]
6> [1,2,3,4,5] -- [1,2,3].  %% -- 是移除操作符。
[4,5]
7> [2,4,2] -- [2,4].
[2]
8> [2,4,2] -- [2,4,2].
[]
9> [] -- [1, 3].   %% 如果左边的 List 里不包含需要移除的值，也没事儿。不要拿这种东西来做面试题，这样会没朋友的。
[]
11> hd([1,2,3,4]).  
1
12> tl([1,2,3,4]).
[2,3,4]
```
上面 hd/1 是取 Head 函数。tl/1 是取 Tail. 这俩都是 BIF (Built-In-Function)，就是 Erlang 内置函数. 
第一行里你也看到了，List 的追加操作会有性能损耗 (lists:append/2 跟 ++ 是一回事儿)，所以我们需要一个从头部插入 List 的操作: 
```shell
13> List = [2,3,4].
[2,3,4]
14> NewList = [1|List].   %% 注意这个 | 的左边应该放元素，右边应该放 List。
[1,2,3,4]
15> [1, 2 | [0]].   %% 左边元素有好几个的话，erlang 会帮你一个一个的插到头部。先插 2，后插1.
[1,2,0]
16> [1, 2 | 0].     %%  右边放的不是 List，这种叫 'improper list'。
%% 虽然你可以生成这种列表，但不要这么做，代码里出现这种一般就是个 bug。忘了这种用法吧。
[1,2|0]

20> [1 | []].       %% List 可以分解为 [ 第一个元素 | 剩下的 List ]。仔细看一下这几行体会一下。
[1]
21> [2 | [1 | []]].
[2,1]
22> [3 | [2 | [1 | []] ] ].
[3,2,1]
```

##### List Comprehensions
实践中我们经常会从一个 List 中，取出我们需要的那些元素，然后做处理，最后再将处理过的元素重新构造成一个新的元素。
你马上就想到了 map，reduce。在 Erlang 里，我们可以用  List Comprehensions 语法，很方便的做一些简单的处理。
``` shell
1> [2*N || N <- [1,2,3,4]].   %% 取出  [1,2,3,4] 中的每个元素，然后乘2，返回值再组成一个新的 List
[2,4,6,8]
2> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].   %% 取出右边列表里所有偶数。
[2,4,6,8,10]
```

##### Anonymous functions 
让我们定义一个函数：
```shell
Add = fun (A, B) -> A + B end.
```
上面的代码里，我们用 fun() 定义了一个 匿名函数, 接收两个参数，并将两个参数的和作为返回值。
最后将这个函数 bind 到 Add 变量:
```shell
1> Add = fun (A, B) -> A + B end.
#Fun<erl_eval.12.118419387>
2> Add(1, 2).
3
```

### Modules
本章代码在：https://github.com/terry-xiaoyu/learn-erlang-in-30-mins/tree/master/modules

Erlang Shell 是一个快速尝试新想法的地方，但我们真正的代码是要写到文件里，然后参与编译的。

Erlang 里代码是用 Module 组织的。一个 Module 包含了一组功能相近的函数。
用一个函数的时候，要这么调用：`Module:Function(arg1, arg2)`。
或者你先 `import` 某个 Module 里的函数，然后用省略Module名的方式调用：`Function(arg1, arg2)`。

Module 可也提供代码管理的作用，加载一个 Module 到 Erlang VM就加载了那个 Module 里的所有代码，然后你想热更新代码的话，直接更新这个 Module 就行了。

来看 Erlang 自带的几个 Module：
```
1> erlang:element(2, {a,b,c}).
b
2> element(2, {a,b,c}).
b
3> lists:seq(1,4).
[1,2,3,4]
4> seq(1,4).
** exception error: undefined shell command seq/2
```
上面的例子里，你能直接用 `erlang` Module 里的 element/2 函数，是因为 erlang 里的常用函数会被 潜在的 `import` 过来。其他的 Module 比如 lists 不会.

`erlang` module 里的函数叫做 `BIF`.

**使用 Module 写 functions:**
建立一个名为 useless.erl 的文件。
在文件的第一行, 用 -module(useless) 来声明你的 module name。注意跟 java 类似，module 名要跟文件名一样。
然后你在你的 module 里写你的函数：
```erlang
-module(useless).
-export([add/2, add/3]). %% export 是导出语法，指定导出 add/2, add/3 函数。没导出的函数在 Module 外是无法访问的。

add(A, B) ->
  A + B.
add(A, B, C) ->
  A + B + C.
```
然后你用 erlc 编译
```shell
mkdir -p ./ebin
erlc -o ebin useless.erl
```

编译后的 beam 文件会在 ebin 目录下，然后你启动 erlang shell：
``` shell
$ erl -pa ./ebin
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> useless:add(1, 2).
3
2> useless:add(1, 2, 1).
4
```

erl -pa 参数的意思是 `Path Add`, 添加目录到 erlang 的 beam 文件查找目录列表里。
就是说，你运行  useless:add(1, 2). 的时候，erlang 发现 module 'useless' 没加载，就在那些查找目录里找 useless.beam，然后加载进来。

Erlang 里面函数是用 函数名/参数个数来表示的，如果两个函数的函数名与参数个数都一样，他们就是一个函数的两个分支，必须写在一起，分支之间用分号分割。
上面的 add(A, B) 可以叫做 add/2, 而 add(A, B, C) 函数叫做 add/3. 注意这个 add/3和 add/2 因为参数个数不一样，所以被认为两个不同的函数，即使他们的函数名是一样的。
所以，第一个函数用 `.` 结尾。如果是一个函数的多个 clause, 是要用 `;` 分割的：
```erlang
-module(clauses).
-export([add/2]).

%% goes into this clause when both A and B are numbers
add(A, B) when is_number(A), is_number(B) ->
  A + B;
%% goes this clause when both A and B are lists
add(A, B) when is_list(A), is_list(B) ->
  A ++ B.
%% crashes when no above clauses matched.
```
上面代码里，定义了一个函数：add/2. 这个函数有两个 clause 分支，一个是计算数字相加的，一个是计算字符串相加的。
代码里 `when` 是一个 `Guard` 关键字。`Pattern Matching` 和 `Guard` 后面讲解。
运行 add/2 时会从上往下挨个匹配：
```shell
$ erl -pa ebin/
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> clauses:add("ABC", "DEF").  %% 第一个 clause 没匹配上。走的是第二个 clause。
"ABCDEF"
2> clauses:add(1, 2).  %% 走第一个 clause
3
3> clauses:add(1, 2.4).
3.4
4> clauses:add(1, "no").  %% 两个 clause 都没匹配上，崩溃了。
** exception error: no function clause matching clauses:add(1,"no") (clauses.erl, line 4)
```


### 常用知识点
##### Pattern Matching

Erlang 里到处都用匹配的。

**1. case clauses**
下面的代码里，我们定义了一个 greet/2 函数
```erlang
-module(case_matching).
-export([greet/2]).

greet(Gender, Name) ->
  case Gender of
    male ->
      io:format("Hello, Mr. ~s!~n", [Name]);
    female ->
      io:format("Hello, Mrs. ~s!~n", [Name]);
    _ ->
      io:format("Hello, ~s!~n", [Name])
  end.
```
case 的各个分支是自上往下依次匹配的，如果 Gender 是 atom 'male', 则走第一个，如果是 'female' 走第二个，如果上面两个都没匹配上，则走第三个。

```shell
$ erl -pa ebin/
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> case_matching:greet(male, "Shawn").
Hello, Mr. Shawn!
ok
```

**2. function clauses**
我们把上面的例子改一下，让代码更规整一点：
```erlang
-module(function_matching).
-export([greet/2]).

greet(male, Name) ->
  io:format("Hello, Mr. ~s!~n", [Name]);
greet(female, Name) ->
  io:format("Hello, Mrs. ~s!~n", [Name]);
greet(_, Name) ->
  io:format("Hello, ~s!~n", [Name]).
```
这个 function 有三个 clause，与 case 一样，自上往下依次匹配。
```shell
$ erl -pa ebin/
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> function_matching:greet(female, "Scarlett").
Hello, Mrs. Scarlett!
ok
2>
```

##### 在匹配中获取值
``` shell
3> {X, 1, 5} = {2, 1, 5}.     %% 如果匹配成功的话，将对应的值 bind 到 X 上。
{2,1,5}
4> X. 
2
5> [H | T] = [1, 2, 3].       %% 现在我们使用匹配来解析 List，将第一个元素绑定到 H, 将其余绑定到 T。
[1,2,3]
6> H.
1
7> T.
[2,3]

8> [_ | T2] = T.      %% 我可以一直这么做下去
[2,3]
9> T2.
[3]
10> [_ | T3] = T2.    %% 再来
[3]
11> T3.
[]
12> f().              %% Erlang 里面变量是 immutable 的，所以我们现在解绑一下所有变量，清理之前用过的变量名。
ok
13> Add = fun({A, B}) -> A + B end.  %% 我们重新定义了 Add 函数，现在它只接收一个 tuple 参数
%% 然后在参数列表里我们做了 pattern matching 以获取 tuple 中的两个值，解析到 A，B.
#Fun<erl_eval.6.118419387>
14> Add({1, 2}).   
3
```

好了，就问你厉不厉害？

##### Guards
前面有用过 `when`, 提到过 guards. 现在我们来认真讨论它：
learn-you-some-erlang 的作者那边 16岁才能"开车" (笑). 那我们写个函数判断一下，某个人能不能开车？
```erlang
old_enough(0) -> false;
old_enough(1) -> false;
old_enough(2) -> false;
...
old_enough(14) -> false;
old_enough(15) -> false;
old_enough(_) -> true.
```
上面这个又点太繁琐了，所以我们得另想办法：
```erlang
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.
```
然后作者又说了，超过 104 岁的人，禁止开车：
``` erlang
right_age(X) when X >= 16, X =< 104 ->   %% 注意这里用了逗号，表示 and
   true;
right_age(_) ->
   false.
```
`when` 语句里，`,`表示 `and`, `;` 表示 `or`, 如果你想用短路运算符的话，用 `andalso` 和`orelse`,  这么写：
``` erlang
right_age(X) when X >= 16 andalso X =< 104 -> true;
```

##### Records
前面讲过 `tagged tuple`，但它用起来还不够方便，因为没有个名字，也不好访问其中的变量。
我们来定义一个好用点的 `tagged tuple`，Erlang 里就是`record`：
``` erlang
-module(records).
-export([get_user_name/1,
         get_user_phone/1]).

-record(user, {
  name,
  phone
}).

get_user_name(#user{name=Name}) ->
  Name.

get_user_phone(#user{phone=Phone}) ->
  Phone.
```

```shell
$ erl
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> c(records).   %% 这是编译 erlang 代码的另外一种办法。c/1 编译并加载 module。
{ok,records}
2> rr(records).  %% 将 records module 中的所有 record 都加载到 erl shell 里。
[user]
4> Shawn = #user{name = <<"Shawn">>, phone = <<"18253232321">>}.
#user{name = <<"Shawn">>,phone = <<"18253232321">>}
5> records:get_user_phone(Shawn).
<<"18253232321">>
6> records:get_user_name(Shawn).
<<"Shawn">>

%% record 其实就是个 tagged tuple, 第一个元素是 record 名字。
7> records:get_user_name({user, <<"Shawn">>, <<"18253232321">>}).
<<"Shawn">>

9> Shawn#user.name.
<<"Shawn">>
10> #user.name.
2
```
你看到 `#user{}` 其实只是一个第一个元素为 `user` 的 tagged tuple `{user, name, phone}`, 而 #user.name 是这个 tuple 里 `name` 字段的位置号 2。
`注意: Erlang 里面的位置、Index 等都是约定从 1 开始的。`

 Shawn#user.name 的意思是取 Shawn 里的第 2 个元素。

### 递归
Erlang 是函数式语言，变量 immutable 的，所以没有 while loop。因为不能让你定义一个递增的 counter 变量。
所以我们用递归来解决大多数问题。
先来一个计算 List 长度的函数：
```erlang
len([]) -> 0;    %% 空列表的长度是 0
len([_|T]) -> 1 + len(T)   %% 列表的长度，是 1 + 剩余列表的长度。
```

简单吧？但是你知道的，这样子如果要计算的 List 长度太长的话，调用栈就特别长，会吃尽内存。计算过程是这样的:
```erlang
len([1,2,3,4]) = len([1 | [2,3,4])
               = 1 + len([2 | [3,4]])
               = 1 + 1 + len([3 | [4]])
               = 1 + 1 + 1 + len([4 | []])
               = 1 + 1 + 1 + 1 + len([])
               = 1 + 1 + 1 + 1 + 0
               = 1 + 1 + 1 + 1
               = 1 + 1 + 2
               = 1 + 3 
               = 4
```

所以我们必须用 `Tail Recursion` (尾递归) 来改写一下:
```erlang
len(L) -> len(L,0).   %% 这其实只是给 len/2 的第二个参数设置了一个默认值 0.
 
len([], Acc) -> Acc;  %% 所有的元素都读完了
len([_|T], Acc) -> len(T,Acc+1).  %% 读一个元素，Acc 增1，然后计算剩下的 List 的长度。
```
尾递归就是，最后一个语句是调用自身的那种递归。Erlang 遇到这总递归的时候，不会再保留调用栈。这样的递归相当于一个 while loop。

我们用 Acc 来记录每次计算的结果，读取一个元素 Acc 就增 1，一直到读取完所有的元素。

第一个例子里，第二个 clause 的最后一个调用是 `1 + len(T)` ，这不是尾递归。因为系统还要保留着调用栈，等其算出 len(T) 之后，再回来跟 1 做加法运算。只有 ` len(T,Acc+1).` 这种才是。


**尾递归与递归的区别：**
有个比喻可以帮你理解他们的差异。
假设玩一个游戏，你需要去收集散落了一路，并通向远方的硬币。

于是你一个一个的捡，一边捡一边往前走，但是你必须往地上撒些纸条做记号，因为不做记号你就忘了回来的路。于是你一路走，一路捡，一路撒纸条。等你捡到最后一个硬币时，你开始沿着记号回来了，一路走，一路捡纸条(保护环境)。等回到出发点时，你把硬币装你包里，把纸条扔进垃圾桶。
这就是**非尾递归**，纸条就是你的调用栈，是内存记录。

下次再玩这个游戏时，你学聪明了，你直接背着包过去了，一路走，一路捡，一路往包里塞。等到了终点时，最后一个硬币进包了，任务完成了，你不回来了！
这就是**尾递归**，省去了调用栈的消耗。

**书接下文**：[30 分钟学 Erlang (二)](https://www.jianshu.com/p/5b7e73576dcb)