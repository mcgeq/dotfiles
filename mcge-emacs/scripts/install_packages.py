"""Install Packages"""
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#    Copyright (C) 2024 mcge. All rights reserved.
# Author:         mcge
# Email:          <mcgeq@outlook.com>
# File:           install_packages.py
# Description:    install packages
# Create   Date:  2024-09-09 23:15:22
# Last Modified:  2024-09-09 23:18:52
# Modified   By:  mcge <mcgeq@outlook.com>

import subprocess
import sys

def install(package: str) -> None:
    try:
        __import__(package)
    except ImportError:
        print(f'Installing {package}.....')
        subprocess.check_call([sys.executable, "-m", "pip", "install", package])

if __name__ == "__main__":
    install('flask')
    install('jieba')
