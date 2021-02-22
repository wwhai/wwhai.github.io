---
title: Springboot项目中使用Mybatisplus分页Mongodb
date:  2020-01-07 17:41:14
index_img: /static/23.jpg
tags: 
- java
- mongodb
- springboot

categories: 
- java

author: wangwenhai
---
本文作者：[wangwenhai] # 概要：本文主要讲一下如何让Springboot项目中的MongoDB查询到的数据按照MybatisPlus的格式返回分页.
<!-- more -->

## 0.概述

本文主要以一个WEB系统站内通知为主,讲一下Springboot项目同时使用MongoDB和Mybatisplus的时候,如何返回统一的分页数据.

## 1.文档结构

```java
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
public class InternalMessage implements Serializable {

    private String id;

    private Integer type;

    private String content;

    private String title;

    private Integer marked;

    private Long userId;

    /**
     * 创建时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;


}
```



## 2.Service

```java
package com.ezlinker.app.modules.internalmessage.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.ezlinker.app.modules.internalmessage.model.InternalMessage;
import org.springframework.data.domain.Pageable;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author lc5900
 * @since 2019-11-13
 */
public interface InternalMessageService {
    /**
     * @param entity
     */
    void save(InternalMessage entity);
    /**
     * 从MongoDB查询站内信
     *
     * @param userId 用户
     * @param marked 是否标记阅读
     * @return
     */
    IPage<InternalMessage> queryForPage(Long userId, Integer marked, Pageable pageable);

}
```



## 3.Service实现类

```java
package com.ezlinker.app.modules.internalmessage.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.ezlinker.app.modules.internalmessage.model.InternalMessage;
import com.ezlinker.app.modules.internalmessage.service.InternalMessageService;
import org.bson.types.ObjectId;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.List;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author lc5900
 * @since 2019-11-13
 */
@Service
public class InternalMessageServiceImpl implements InternalMessageService {


    @Resource
    MongoTemplate mongoTemplate;

    @Override
    public void save(InternalMessage entity) {
        mongoTemplate.insert(entity, "internal_message");
    }

    //最主要的是这里,需要返回一个Ipage匿名实现对象
    //根据查询条件来统计Page各种属性
    @Override
    public IPage<InternalMessage> queryForPage(Long userId, Integer marked, Pageable pageable) {
        Query query = new Query();
        query.addCriteria(Criteria.where("userId").is(userId));
        query.addCriteria(Criteria.where("marked").is(marked));
        query.with(Sort.by(Sort.Direction.DESC, "createTime"));
        query.with(pageable);

        List<InternalMessage> list = mongoTemplate.find(query, InternalMessage.class, "internal_message");
        long total = mongoTemplate.count(query, "internal_message");

        return new IPage<InternalMessage>() {
            @Override
            public List<OrderItem> orders() {
                return OrderItem.descs("createTime");
            }

            @Override
            public List<InternalMessage> getRecords() {
                return list;
            }

            @Override
            public IPage<InternalMessage> setRecords(List<InternalMessage> records) {
                return this;
            }

            @Override
            public long getTotal() {
                return total;
            }

            @Override
            public IPage<InternalMessage> setTotal(long total) {
                return this;
            }

            @Override
            public long getSize() {
                return pageable.getPageSize();
            }

            @Override
            public IPage<InternalMessage> setSize(long size) {
                return this;
            }

            @Override
            public long getCurrent() {
                return pageable.getPageNumber();
            }

            @Override
            public IPage<InternalMessage> setCurrent(long current) {
                return this;
            }
        };
    }

}
```



## 4.控制器

```java
package com.ezlinker.app.modules.internalmessage.controller;

import com.ezlinker.app.common.web.CurdController;
import com.ezlinker.app.modules.internalmessage.model.InternalMessage;
import com.ezlinker.app.modules.internalmessage.service.InternalMessageService;
import com.ezlinker.app.common.exception.XException;
import com.ezlinker.app.common.exchange.R;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

/**
 * <p>
 * 站内信 前段控制器
 * </p>
 *
 * @author lc5900
 * @since 2019-11-13
 */
@RestController
@RequestMapping("/internalMessages")
public class InternalMessageController extends CurdController<InternalMessage> {
    @Autowired
    private InternalMessageService internalMessageService;

    public InternalMessageController(HttpServletRequest httpServletRequest) {
        super(httpServletRequest);
    }

    /**
     * 分页检索
     *
     * @param current 页码：必传
     * @param size    页长：必传
     * @return
     * @throws XException
     */
    @GetMapping
    public R queryForPage(
            @RequestParam Integer current,
            @RequestParam Integer marked,
            @RequestParam Integer size) throws XException {
        Pageable pageable = PageRequest.of(current, size, Sort.by(Sort.Direction.DESC, "_id"));

        return data(internalMessageService.queryForPage(getUserDetail().getId(), marked, pageable));
    }

}
```

通过:

Pageable pageable = PageRequest.of(current, size, Sort.by(Sort.Direction.DESC, "_id"));

构建Page对象,然后传给Service去处理,最终生成MongoDb'的query语句.

## 5.总结

本文主要讲了如何实现Mongodb兼容Mybatisplus进行分页.