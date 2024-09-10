"""Flask jieba"""
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#    Copyright (C) 2024 mcge. All rights reserved.
# Author:         mcge
# Email:          <mcgeq@outlook.com>
# File:           segment.py
# Description:    分词
# Create   Date:  2024-09-09 23:40:43
# Last Modified:  2024-09-10 21:00:10
# Modified   By:  mcge <mcgeq@outlook.com>

from flask import Flask, request, jsonify
import jieba

# jieba.setLogLevel(jieba.logging.ERROR)

app = Flask(__name__)

@app.route('/segment', methods=['POST'])
def segment_text():
    data = request.json
    text = data.get('text', '')
    if not text:
        return jsonify([])  # 如果没有提供文本，返回一个空列表
    words = jieba.lcut(text)
    return jsonify(words)

if __name__ == '__main__':
    app.run(host='127.0.0.1', port=20193)
