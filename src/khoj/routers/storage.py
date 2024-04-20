import logging
import os
import uuid

logger = logging.getLogger(__name__)

AWS_ACCESS_KEY = os.getenv("AWS_ACCESS_KEY")
AWS_SECRET_KEY = os.getenv("AWS_SECRET_KEY")
AWS_UPLOAD_IMAGE_BUCKET_NAME = os.getenv("AWS_IMAGE_UPLOAD_BUCKET")

aws_enabled = AWS_ACCESS_KEY is not None and AWS_SECRET_KEY is not None and AWS_UPLOAD_IMAGE_BUCKET_NAME is not None

if aws_enabled:
    from boto3 import client

    s3_client = client("s3", aws_access_key_id=AWS_ACCESS_KEY, aws_secret_access_key=AWS_SECRET_KEY)


def upload_image(image: bytes, user_id: uuid.UUID):
    """Upload the image to the S3 bucket"""
    if not aws_enabled:
        logger.info("AWS is not enabled. Skipping image upload")
        return None

    image_key = f"{user_id}/{uuid.uuid4()}.webp"
    try:
        s3_client.put_object(Bucket=AWS_UPLOAD_IMAGE_BUCKET_NAME, Key=image_key, Body=image, ACL="public-read")
        url = f"https://{AWS_UPLOAD_IMAGE_BUCKET_NAME}.s3.amazonaws.com/{image_key}"
        return url
    except Exception as e:
        logger.error(f"Failed to upload image to S3: {e}")
        return None
