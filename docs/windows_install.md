# Windows Installation

These steps can be used to setup Khoj on a clean, new Windows 11 machine. It has been tested on a Windows VM

## Prerequisites
1. Ensure you have Visual Studio C++ Build tools installed. You can download it [from Microsoft here](https://visualstudio.microsoft.com/visual-cpp-build-tools/). At the minimum, you should have the following configuration:
    <img width="1152" alt="Screenshot 2023-07-12 at 3 56 25 PM" src="https://github.com/khoj-ai/khoj/assets/65192171/b506a858-2f5e-4c85-946b-5422d83f112a">
2. Ensure you have Python installed. You can check by running `python --version`. If you don't, install the latest version [from here](https://www.python.org/downloads/).
    - Ensure you have pip installed: `py -m ensurepip --upgrade`.

## Quick start
1. Open a PowerShell terminal.
2. Run `pip install khoj-assistant`
3. Start Khoj with `khoj`

## Installation in a Virtual Environment
Use this if you want to install with a virtual environment. This will make it much easier to manage your dependencies. You can read more about [virtual environments](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/) here.

1. Open a PowerShell terminal with the `Run as Administrator` privileges.
2. Create a virtual environment: `mkdir khoj && cd khoj && py -m venv .venv`
3. Activate the virtual environment: `.\.venv\Scripts\activate`. If you get a permissions error, then run `Set-ExecutionPolicy -ExecutionPolicy RemoteSigned`.
4. Run `pip install khoj-assistant`
5. Start Khoj with `khoj`
