---
title: 一个非常简单的解释器
date: 2022-06-01 20:52:43
index_img: /static/18.jpg
tags:
- C 编程
---

#  一个非常简单的解释器:BrainFuck

```c
#include <stdio.h>
#include <stdlib.h>
//
// 此处的是Brainfuck源码，从文件里面读
//
char program[] = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.";
// 内存大小默认4k
int data[1024 * 4];
int pc = 0;
char token = 0;
int current_pos = 0;
int stack = 0;
//-----------------------------------------------------------------
int main(int argc, char *v[])
{
    while ((token = program[pc]))
    {
        if (token == '>')
        {
            current_pos++;
        }
        if (token == '<')
        {
            current_pos--;
        }
        if (token == '+')
        {
            data[current_pos]++;
        }
        if (token == '-')
        {
            data[current_pos]--;
        }
        if (token == '.')
        {
            putc(data[current_pos],
                 stdout);
        }
        if (token == ',')
        {
            data[current_pos] = getc(stdin);
        }
        while (token == '[' && !data[current_pos])
        {
            if (program[pc] == '[')
            {
                stack++;
            }
            if (program[pc] == ']' && stack-- == 1)
            {
                break;
            }
            pc++;
        }
        while (token == ']' && data[current_pos])
        {
            if (program[pc] == ']')
            {
                stack++;
            }
            if (program[pc] == '[' && stack-- == 1)
                break;
            pc--;
        }
        pc++;
    }
}
```

可玩玩这个：https://ashupk.github.io/Brainfuck/brainfuck-visualizer-master/index.html
图形化演示，比较有助于理解。