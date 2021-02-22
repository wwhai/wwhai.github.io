---
title: Java8Lambda表达式写法
date:  2019-11-19 22:54:51
index_img: /static/3.jpg
tags: 
- Java

categories: 
- Java

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：Java8的lambda的简单实践
<!-- more -->
     java8支持lambda表达式，常见的一些操作都可以通过lambda来实现，比如遍历List，或者是实现某个事件接口，还有我们熟悉的Swing的事件，如下案例所示：

```java
// Java 8之前：
JButton b1=  new JButton("BeforeJava8");
b.addActionListener(new ActionListener() {
    @Override
    public void actionPerformed(ActionEvent e) {
    System.out.println("Button push");
    }
});

// Java 8方式：
JButton b2=  new JButton("Java8");
b2.addActionListener((e) -> {
    System.out.println("Button push");
});
```

       有时候我们比较迷惑：Java明明是OOP语言，Lambda表达式岂不是破坏了OOP的封装特性？实则不然，Lambda其实是用接口实现的，而这个接口只有一个方法。我们看下面的案例：

```java
/**
 * @author wangwenhai
 */
interface PrintHandler {
    void print(int[] array);
}


public class Main {
    static void print(int[] array, PrintHandler printHandler) {
        printHandler.print(array);
    }

    public static void main(String[] args) {
        int array[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        print(array, new PrintHandler() {
            @Override
            public void print(int[] array) {
                for (int i = 0; i < array.length; i++) {
                    System.out.println("数组:" + i);
                }
            }
        });
    }
}

```

这是一个简单的遍历数组的写法，通过传一个接口进去，然后用户去实现具体的遍历方法，上面看上去是一个很普通的匿名接口实现，但是如果我们用了Java8以后，这段代码会变得更加精简：

```java
/**
 * @author wangwenhai
 */
interface PrintHandler {
    void print(int[] array);
}


public class Main {
    private static void print(int[] array, PrintHandler printHandler) {
        printHandler.print(array);
    }

    public static void main(String[] args) {
        int[] array = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        print(array, array1 -> {
            for (int i = 0; i < array1.length; i++) {
                System.out.println("数组:" + i);
            }
        });
    }
}

```

      大家也许已经注意到了，此处的匿名接口实现不见了，仿佛出现了一段从没见过的代码，其实这就是Java8的lambda的实现：

```java
        print(array, array1 -> {
            for (int i = 0; i < array1.length; i++) {
                System.out.println("数组:" + i);
            }
        });
```

        其中array1是传给接口的参数，在这里直接省略了接口的实现过程，因为就一个方法，所以这个参数直接被传进print方法里面。

        我们用个图来表示从OOP到FP（函数式编程）的过程。

![](https://oscimg.oschina.net/oscnet/2f08422ddd760c12aafcfc9e784d5c1e2db.jpg)