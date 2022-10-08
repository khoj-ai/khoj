#!/usr/bin/env python

from setuptools import find_packages, setup

from pathlib import Path
this_directory = Path(__file__).parent

setup(
    name='khoj-assistant',
    version='0.1.10',
    description="A natural language search engine for your personal notes, transactions and images",
    long_description=(this_directory / "Readme.md").read_text(encoding="utf-8"),
    long_description_content_type="text/markdown",
    author='Debanjum Singh Solanky, Saba Imran',
    author_email='debanjum+pypi@gmail.com, narmiabas@gmail.com',
    url='https://github.com/debanjum/khoj',
    license="GPLv3",
    keywords="search semantic-search productivity NLP org-mode markdown beancount images",
    python_requires=">=3.8, <4",
    packages=find_packages(
        where=".",
        exclude=["tests*"],
        include=["src*"]
    ),
    install_requires=[
        "numpy == 1.22.4",
        "torch == 1.12.1",
        "torchvision == 0.13.1",
        "transformers == 4.21.0",
        "sentence-transformers == 2.1.0",
        "openai == 0.20.0",
        "huggingface_hub == 0.8.1",
        "pydantic == 1.9.1",
        "fastapi == 0.77.1",
        "uvicorn == 0.17.6",
        "jinja2 == 3.1.2",
        "pyyaml == 6.0",
        "pytest == 7.1.2",
        "pillow == 9.2.0",
        "aiofiles == 0.8.0",
        "dateparser == 1.1.1",
        "pyqt6 == 6.3.1",
        "defusedxml == 0.7.1",
    ],
    include_package_data=True,
    entry_points={"console_scripts": ["khoj = src.main:run"]},
    classifiers=[
        "Development Status :: 4 - Beta",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
    ]
)
