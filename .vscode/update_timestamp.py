# update_timestamp.py

import re
import sys
from datetime import datetime

def update_timestamp(file_path):
    # 获取当前时间
    current_time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    # 读取文件内容
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.readlines()

    # 更新 Last Modified 字段
    updated_content = []
    for line in content:
        # 查找并替换包含 Last Modified 的行，同时保留前面的内容
        if 'Last Modified:' in line:
            line = re.sub(r'(.*Last Modified: ).*', r'\1 ' + current_time, line)
        updated_content.append(line)

    # 将更新后的内容写回文件
    with open(file_path, 'w', encoding='utf-8') as file:
        file.writelines(updated_content)

# 示例使用
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python update_timestamp.py <file_path>")
    else:
        update_timestamp(sys.argv[1])
