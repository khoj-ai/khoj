import os

from khoj.processor.content.images.image_to_entries import ImageToEntries


def test_png_to_jsonl():
    with open("tests/data/images/testocr.png", "rb") as f:
        image_bytes = f.read()
    data = {"tests/data/images/testocr.png": image_bytes}
    entries = ImageToEntries.extract_image_entries(image_files=data)
    assert len(entries) == 2
    assert "opencv-python" in entries[1][0].raw


def test_jpg_to_jsonl():
    with open("tests/data/images/nasdaq.jpg", "rb") as f:
        image_bytes = f.read()
    data = {"tests/data/images/nasdaq.jpg": image_bytes}
    entries = ImageToEntries.extract_image_entries(image_files=data)
    assert len(entries) == 2
    assert "investments" in entries[1][0].raw
