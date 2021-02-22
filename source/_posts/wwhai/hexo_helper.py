'''
自己的Hexo博客辅助工具
主要功能：生成重复性的模板,命令行操作如下:
hexo_helper  -n [N] -t [T] -c [C]
n:文章的名字
t:Tag标签
c:分类
一个完整的命令如下所示:
python3 ./hexo_helper.py -n "helloworld" -t "Python" -c "Python"
'''
def gen():
    from datetime import datetime
    import argparse
    import random
    parser = argparse.ArgumentParser()
    parser.add_argument("-n", dest="n", help="文章标题")
    parser.add_argument("-c", dest="c", help="文章分类,如有多个用空格分隔,例如:'java c c++'")
    parser.add_argument("-t", dest="t", help="文章Tag,如有多个用空格分隔,例如:'java c c++'")
    parser.add_argument("-v",  action='version', help='HexoHelperV0.0.1@wwhai')
    args = parser.parse_args()
    #
    name = args.n
    tag = args.t
    categories = args.c
    #
    date_time = datetime.now()
    str_date = date_time.strftime('%Y%m%d')
    date = date_time.strftime('%Y-%m-%d %H:%M:%S')

    title = name
    # 解析分类
    categorie_list = categories.split(" ")
    # 解析标签
    tag_list = tag.split(" ")
    # 生成模板
    template = '''
---
title: ${TITLE}
date:  ${D}
index_img: /static/${IMG}.jpg
tags: 
${TAGS}
categories: 
${C}
author: wangwenhai
---
本文作者：[wangwenhai] # 概要：概要->
<!-- more -->
'''
    tag_yml = ""
    categorie_yml = ""
    # 构成tag
    for tag in tag_list:
        tag_yml += "- "+tag+"\n"
    # 构成categorie
    for categorie in categorie_list:
        categorie_yml += "- "+categorie+"\n"


    template = template.replace("${TITLE}", title).replace("${D}", date).replace(
        "${TITLE}", title).replace("${TAGS}", tag_yml).replace("${C}", categorie_yml).replace("${IMG}", str(random.randint(1, 25)))
    print("文章模板生成中......")

    with open('./' +str_date + "-" + name+'.md', mode='w', encoding='utf-8') as markdown_file:
        markdown_file.writelines(template)
    print("文章生成成功!")




def rename():
    import os
    import re
    import sys
    import datetime
    import time
    # 文件位置
    file_folder = "C:\\Users\\wangwenhai\\Pictures\\"
    # 新文件的位置
    new_file_folder = "C:\\Users\\wangwenhai\\Pictures\\1\\"

    i = 1
    for old_name in os.listdir(file_folder): 
        print("Rename file:",file_folder + old_name)
        if os.path.isdir(file_folder + old_name):
                continue
        i+=1
        new_file_name = time.strftime('%Y%m%d'+str(i),  time.localtime() ) + "_" + old_name #old_name[-4:]
        os.rename(file_folder + old_name, os.path.join(new_file_folder, new_file_name)) 
    return True
# 文章头部加入内容
def insert():
    import os
    import random
    import re
    import sys
    from datetime import datetime
    import time
    # 文件位置
    file_folder = "D:\\github\\HexoBlog\\source\\_posts\\wwhai\\Erlang程序设计\\"

    i = 1
    for old_name in os.listdir(file_folder): 
        print("更新文件:",file_folder + old_name)
        if os.path.isdir(file_folder + old_name):
                continue
        date_time = datetime.now()
        str_date = date_time.strftime('%Y%m%d')
        date = date_time.strftime('%Y-%m-%d %H:%M:%S')
        header  ="---\ntitle: " + old_name[0:-3] + "\ndate: " + date +"\nindex_img: /static/"+ str(random.randint(1, 58))+".jpg" + "\n"  + """
tags: 
- Erlang
- OTP
categories: 
- Erlang程序设计
---

概要：Erlang程序设计基础知识讲解
<!-- more -->
申明:此系列文章来源于【https://gitee.com/yujian1018/erlang.git】,经过本人搬运整理，并非原作，希望作者收到我的邮件以后联系本人。如有侵权会及时删除。
<hr>
"""
        with open(file= file_folder + old_name, mode= 'r+',encoding="UTF-8") as f:
            content = f.read()  
            f.seek(0, 0)
            f.write(header + content )

if __name__ == '__main__':
    gen()
